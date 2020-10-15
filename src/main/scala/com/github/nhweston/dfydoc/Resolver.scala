package com.github.nhweston.dfydoc

import java.nio.file.Paths

import com.github.nhweston.dfydoc.Resolver._
import com.github.nhweston.dfydoc.node._

import scala.annotation.tailrec
import scala.jdk.javaapi.CollectionConverters.asScala

case class Resolver(
  rootPath: String,
  files: Seq[SrcFile],
) {

  lazy val rootPathAbs = Paths.get(rootPath).toRealPath()

  lazy val root: SrcDir = {
    val root = SrcDir(rootPathAbs.getFileName.toString, true, Map.empty)
    files.foldLeft(root) { case (dir, file) =>
      val path = Paths.get(rootPathAbs.toString, file.path)
      val names = asScala(rootPathAbs.relativize(path).iterator()).map(_.toString).toSeq
      dir.create(file, names).asInstanceOf[SrcDir]
    }
  }

  lazy val (tokensToPaths, tokensToNodes): (TokensToPaths, TokensToNodes) = {
    type Result = (TokensToPaths, TokensToNodes)
    val bttp = Map.newBuilder[Token, DeclPath]
    val bttn = Map.newBuilder[Token, Resolvable]

    def aux0(sp: SrcPath, path: DeclPath): Unit =
      sp match {
        case SrcDir(name, isRoot, sub) =>
          val f =
            if (isRoot) aux0(_, DeclPath())
            else aux0(_, DeclPath(Seq(name)))
          for ((_, sp) <- sub) f(sp)
        case sf @ SrcFile(_, decls, _) =>
          for (decl <- decls)
            aux1(decl, path /+ sf.name)
      }

    def aux1(decl: Resolvable, path: DeclPath): Unit = {
      bttp += (decl.token -> (path #+ decl.name))
      bttn += (decl.token -> decl)
      for (child <- decl.children)
        aux1(child, path #+ decl.name)
    }

    aux0(root, DeclPath())
    (bttp.result(), bttn.result())
  }

  def getRelativePath(target: Resolvable, root: Resolvable): String =
    getRelativePath(target.token, root.token)

  def getRelativePath(target: Token, root: Token): String =
    (tokensToPaths.get(target), tokensToPaths.get(root)) match {
      case (Some(tp), Some(rp)) =>
        val tDirs :+ tName = tp.file
        val rDirs :+ _ = rp.file
        @tailrec
        def aux(
          tDirs: Seq[String] = tDirs,
          rDirs: Seq[String] = rDirs,
        ): Seq[String] =
          (tDirs, rDirs) match {
            case (thd +: ttl, rhd +: rtl) if (thd == rhd) =>
              // pop common ancestor
              aux(ttl, rtl)
            case _ =>
              // move up to inner-most common ancestor and append remaining path
              rDirs.map(_ => "..") ++ tDirs
          }
        (aux() :+ tName).mkString("/") + ".html#" + tp.anchor.mkString("~")
      case _ => ""
    }

  def getAnchorName(token: Token): String =
    tokensToPaths.get(token) match {
      case Some(path) => path.anchor.mkString("~")
      case None => ""
    }

  /** Returns token pointed to by a link. */
  def resolveLink(root: Resolvable, path: Seq[String]): Option[Token] = {

    val rootFile = files.find(_.path == root.token.file).get

    @tailrec
    def traverseFromPath(root: Resolvable, path: Seq[String]): Option[Resolvable] =
      path match {
        case hd +: tl =>
          root.children.find(_.name == hd) match {
            case Some(sub) => traverseFromPath(sub, tl)
            case None => None
          }
        case Nil => Some(root)
      }

    @tailrec
    def tryAll(includes: Seq[Resolvable], path: Seq[String]): Option[Resolvable] =
      includes match {
        case hd +: tl =>
          traverseFromPath(hd, path) match {
            case Some(result) => Some(result)
            case None => tryAll(tl, path)
          }
        case Nil => None
      }

    @tailrec
    def tryAncestors(rootPath: Seq[String], path: Seq[String]): Option[Resolvable] = {
      rootPath match {
        case init :+ _ =>
          traverseFromPath(rootFile, rootPath ++ path) match {
            case Some(result) => Some(result)
            case None => tryAncestors(init, path)
          }
        case Nil => None
      }
    }

    // first, try the inner-most scope
    traverseFromPath(root, path)
      // try includes
      .orElse(
        tryAll(
          for {
            includer <- files.filter(_.path == root.token.file)
            includePath <- includer.includes
            include <- files.filter(_.path == includePath)
          } yield include,
          path,
        )
      )
      // try ancestor scopes
      .orElse(tryAncestors(tokensToPaths(root.token).anchor, path))
      .map(_.token)

  }

}

object Resolver {

  type TokensToPaths = Map[Token, DeclPath]
  type TokensToNodes = Map[Token, Resolvable]

  case class DeclPath(
    file: Seq[String] = Seq.empty,
    anchor: Seq[String] = Seq.empty,
  ) {

    def appendFilePath(name: String) =
      copy(file = file :+ name)

    def appendAnchorPath(name: String) =
      copy(anchor = anchor :+ name)

    def /+(name: String) = appendFilePath(name)

    def #+(name: String) = appendAnchorPath(name)

  }

}
