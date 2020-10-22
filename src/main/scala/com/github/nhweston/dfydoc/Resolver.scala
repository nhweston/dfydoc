package com.github.nhweston.dfydoc

import java.io.File
import java.nio.file.Paths

import com.github.nhweston.dfydoc.Resolver._
import com.github.nhweston.dfydoc.node._

import scala.annotation.tailrec
import scala.jdk.javaapi.CollectionConverters.asScala

case class Resolver(
  rootDirPath: String,
  files: Seq[SrcFile],
) {

  lazy val projectName = (new File(rootDirPath)).getName

  implicit val self: Resolver = this

  /** The root directory. Evaluation resolves the directory structure of the Dafny program. */
  lazy val root: SrcDir = {
    val rootDirPathAbs = Paths.get(rootDirPath).toRealPath()
    /** Inserts `file` at `path` relative to `dir`. */
    def insertFile(
      file: SrcFile,
      dir: SrcDir,
      path: Seq[String],
    ): SrcDir =
      (dir, path) match {
        case (SrcDir(_, sub), fileName +: Nil) =>
          dir.copy(sub = sub.updated(fileName, file))
        case (SrcDir(_, sub), dirName +: (tl @ _ +: _)) =>
          dir.copy(
            sub = sub.updatedWith(dirName) {
              case Some(subdir: SrcDir) => Some(insertFile(file, subdir, tl))
              case None => Some(insertFile(file, SrcDir(Some(dirName), Map.empty), tl))
            }
          )
      }
    // start with empty root directory
    val root = SrcDir(None, Map.empty)
    // add all files
    files.foldLeft(root) { case (dir, file) =>
      val pathRaw = Paths.get(rootDirPathAbs.toString, file.path)
      val path = asScala(rootDirPathAbs.relativize(pathRaw).iterator()).map(_.toString).toSeq
      insertFile(file, dir, path)
    }
  }

  lazy val tokensToPaths: Map[Token, DeclPath] = {
    val builder = Map.newBuilder[Token, DeclPath]
    def auxSrc(src: Src, path: DeclPath): Unit =
      src match {
        case SrcDir(name, sub) =>
          for ((_, next) <- sub)
            auxSrc(next, DeclPath(name.toSeq))
        case file @ SrcFile(_, decls, _) =>
          for (decl <- decls)
            auxDecl(decl, path.appendFilePath(file.name))
      }
    def auxDecl(decl: Decl, path: DeclPath): Unit = {
      val pathNext = path.appendAnchorPath(decl.name)
      builder += (decl.token -> pathNext)
      for (child <- decl.children)
        auxDecl(child, pathNext)
    }
    auxSrc(root, DeclPath())
    builder.result()
  }

  val tokensToNodes =
    LazyMap[Token, Decl] { token =>
      val file = files.find(_.path == token.file).get
      val anchor = tokensToPaths(token).anchor
      traverse(file, anchor).get
    }

  /** Returns the containing declaration of `token` or `None` if it is top-level. */
  val parent =
    LazyMap[Token, Option[Decl]] { token =>
      val path = tokensToPaths(token)
      path.anchor match {
        case init :+ _ =>
          val file = files.find(_.path == token.file).get
          traverse(file, init)
        case _ => None
      }
    }

  /** Get the declaration at `path` from `root`. */
  @tailrec
  final def traverse(
    root: Decl,
    path: Seq[String],
  ): Option[Decl] =
    path match {
      case hd +: tl =>
        root.children.find(_.name == hd) match {
          case Some(sub) => traverse(sub, tl)
          case None => None
        }
      case Nil =>
        Some(root)
    }

  /** Get the declaration at `path` from `file`. */
  def traverse(
    file: SrcFile,
    path: Seq[String],
  ): Option[Decl] =
    path match {
      case hd +: tl =>
        file.decls.find(_.name == hd) match {
          case Some(decl) =>
            traverse(decl, tl) match {
              case Some(result) => Some(result)
              case None => None
            }
          case None =>
            println(s"WARNING: Cannot find file $hd")
            None
        }
      case Nil => None
    }

  /** Attempt to traverse to `path` from each file in `files`. Returns the first success. */
  @tailrec
  final def tryTraverseAll(
    files: Seq[SrcFile],
    path: Seq[String],
  ): Option[Decl] =
    files match {
      case hd +: tl =>
        traverse(hd, path) match {
          case Some(result) => Some(result)
          case None => tryTraverseAll(tl, path)
        }
      case Nil => None
    }

  /** Attempt to traverse to `path` from each ancestor of `decl`. */
  def tryTraverseAncestors(
    decl: Decl,
    path: Seq[String],
  ): Option[Decl] = {
    val rootFile = files.find(_.path == decl.token.file).get
    @tailrec
    def aux(
      rootPath: Seq[String] = tokensToPaths(decl.token).anchor,
      targetPath: Seq[String] = path,
    ): Option[Decl] =
      rootPath match {
        case init :+ _ =>
          traverse(rootFile, rootPath ++ path) match {
            case Some(result) => Some(result)
            case None => aux(init, path)
          }
        case Nil =>
          traverse(rootFile, rootPath)
      }
    aux()
  }

  /** Returns token pointed to by a link. */
  def resolveLink(root: Decl, path: Seq[String]): Option[Decl] = {
    // try the inner-most scope
    traverse(root, path)
      // try included files
      .orElse {
        val includes =
          for {
            includer <- files.filter(_.path == root.token.file)
            includePath <- includer.includes
            include <- files.filter(_.path == includePath)
          } yield include
        tryTraverseAll(includes, path)
      }
      // try ancestral scopes
      .orElse(
        parent(root.token) match {
          case Some(parent) =>
            tryTraverseAncestors(parent, path)
          case None =>
            val rootFile = files.find(_.path == root.token.file).get
            traverse(rootFile, path)
        }
      )
  }

}

object Resolver {

  type TokensToPaths = Map[Token, DeclPath]
  type TokensToNodes = Map[Token, Decl]

  case class DeclPath(
    file: Seq[String] = Seq.empty,
    anchor: Seq[String] = Seq.empty,
  ) {

    def appendFilePath(name: String) =
      copy(file = file :+ name)

    def appendAnchorPath(name: String) =
      copy(anchor = anchor :+ name)

    def relativise(root: DeclPath): DeclPath = {
      val DeclPath(tDirs :+ tFile, tAnchor) = this
      val DeclPath(rDirs :+ _, _) = root
      @tailrec
      def aux(
        tDirs: Seq[String] = tDirs,
        rDirs: Seq[String] = rDirs,
      ): Seq[String] =
        (tDirs, rDirs) match {
          case (tHd +: tTl, rHd +: rTl) if (tHd == rHd) =>
            // pop common ancestor
            aux(tTl, rTl)
          case _ =>
            // move to inner-most common ancestor and append remaining path
            rDirs.map(_ => "..") ++ tDirs :+ tFile
        }
      DeclPath(aux(), tAnchor)
    }

    def getFileUrl: String = file.mkString("/")

    def getAnchorUrl: String = anchor.mkString("~")

    def toUrl: String = s"$getFileUrl.html#$getAnchorUrl"

  }

}
