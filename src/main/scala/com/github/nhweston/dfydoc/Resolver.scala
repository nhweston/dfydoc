package com.github.nhweston.dfydoc

import java.io.File
import java.nio.file.Paths

import com.github.nhweston.dfydoc.DocNode.{Tokened, SourceDirectory, SourceFile, Token}
import com.github.nhweston.dfydoc.Resolver.Path

import scala.annotation.tailrec
import scala.jdk.javaapi.CollectionConverters.asScala

class Resolver (
  val pathIn: String,
  val pathOut: String,
  val files: Seq[SourceFile],
) {

  implicit val self: Resolver = this

  lazy val projectName = (new File(pathIn)).getName

  lazy val root: SourceDirectory = {
    val pathAbsolute = Paths.get(pathIn).toRealPath()
    /** Inserts `file` at `path` relative to `directory`. */
    def insertFile(
      file: SourceFile,
      directory: SourceDirectory,
      path: Seq[String],
    ): SourceDirectory = {
      val SourceDirectory(_, contents) = directory
      path match {
        case fileName +: Nil =>
          // insert the file here
          directory.copy(contents = contents.updated(fileName, file))
        case subdirectoryName +: (tl @ (_ +: _)) =>
          // recurse to next directory
          directory.copy(
            contents = contents.updatedWith(subdirectoryName) {
              case Some(subdirectory: SourceDirectory) =>
                // subdirectory already exists
                Some(insertFile(file, subdirectory, tl))
              case None =>
                // subdirectory does not exist, create it
                val subdirectory = SourceDirectory(directory.path :+ subdirectoryName, Map.empty)
                Some(insertFile(file, subdirectory, tl))
              case Some(file: SourceFile) =>
                throw new MatchError(file)
            }
          )
        case Nil =>
          throw new MatchError(Nil)
      }
    }
    // start with empty root directory
    val root = SourceDirectory(Seq.empty, Map.empty)
    // add all files
    files.foldLeft(root) { (directory, file) =>
      val pathRaw = Paths.get(pathAbsolute.toString, file.path.mkString(File.separator))
      val path = asScala(pathAbsolute.relativize(pathRaw).iterator()).map(_.toString).toSeq
      insertFile(file, directory, path)
    }
  }

  lazy val tokensToPaths: Map[Token, Path] = {
    val builder = Map.newBuilder[Token, Path]
    def auxFile(directory: SourceDirectory): Unit = {
      val SourceDirectory(filePath, contents) = directory
      val path = Path(filePath, Seq.empty)
      contents.values.foreach {
        case subdirectory: SourceDirectory =>
          auxFile(subdirectory)
        case file: SourceFile =>
          auxAnchor(file, path.appendFile(file.name))
      }
    }
    def auxAnchor(node: Tokened, path: Path): Unit = {
      builder += (node.token -> path)
      node.children.foreach {
        case child: Tokened =>
          auxAnchor(child, path.appendAnchor(child.name))
        case _ => ()
      }
    }
    auxFile(root)
    builder.result()
  }

  lazy val tokensToNodes: Map[Token, Tokened] = {
    val builder = Map.newBuilder[Token, Tokened]
    def auxDirectory(directory: SourceDirectory): Unit = {
      for (file <- directory.files)
        auxTokened(file)
      for (subdirectory <- directory.subdirectories)
        auxDirectory(subdirectory)
    }
    def auxTokened(node: Tokened): Unit = {
      builder += (node.token -> node)
      for (child <- node.children)
        auxTokened(child)
    }
    auxDirectory(root)
    builder.result()
  }

  lazy val tokensToParents: Map[Token, Tokened] = {
    val builder = Map.newBuilder[Token, Tokened]
    def auxDirectory(directory: SourceDirectory): Unit = {
      for (file <- directory.files)
        auxTokened(file, None)
      for (subdirectory <- directory.subdirectories)
        auxDirectory(subdirectory)
    }
    def auxTokened(node: Tokened, parent: Option[Tokened]): Unit = {
      parent match {
        case Some(parent) =>
          builder += (node.token -> parent)
        case None => ()
      }
      val parentNext =
        node match {
          case parentNext: Tokened => Some(parentNext)
          case _ => parent
        }
      for (child <- node.children)
        auxTokened(child, parentNext)
    }
    auxDirectory(root)
    builder.result()
  }

  @tailrec
  final def traverse(
    root: Tokened,
    path: Seq[String],
  ): Option[Tokened] =
    path match {
      case hd +: tl =>
        root.children.find {
          case child: Tokened => child.name == hd
          case _ => false
        } match {
          case Some(child) => traverse(child, tl)
          case None => None
        }
      case Nil =>
        Some(root)
    }

  def resolveLink(root: Tokened, path: Seq[String]): Option[Tokened] = {
    @tailrec
    def resolveInFile(root: Tokened): Either[SourceFile, Tokened] =
      traverse(root, path) match {
        case Some(result) =>
          Right(result)
        case None =>
          root match {
            case file: SourceFile =>
              Left(file)
            case root =>
              resolveInFile(tokensToParents(root.token))
          }
      }
    @tailrec
    def resolveInIncludes(includes: Seq[Seq[String]]): Option[Tokened] =
      includes match {
        case hd +: tl =>
          traverse(root, hd) match {
            case Some(file: SourceFile) =>
              traverse(file, path) match {
                case Some(node) => Some(node)
                case None => resolveInIncludes(tl)
              }
            case _ => resolveInIncludes(tl)
          }
        case Nil =>
          None
      }
    resolveInFile(root) match {
      case Right(result) => Some(result)
      case Left(file) => resolveInIncludes(file.includes)
    }
  }


}

object Resolver {

  case class Path(
    file: Seq[String],
    anchor: Seq[String],
  ) {

    def appendFile(name: String) =
      copy(file = file :+ name)

    def appendAnchor(name: String) =
      copy(anchor = anchor :+ name)

    def relativize(root: Path): Path = {
      val Path(thisDirectories :+ thisFile, thisAnchor) = this
      val Path(rootDirectories :+ _, _) = root
      @tailrec
      def aux(
        thisDirectories: Seq[String] = thisDirectories,
        rootDirectories: Seq[String] = rootDirectories,
      ): Seq[String] =
        (thisDirectories, rootDirectories) match {
          case (thisHd +: thisTl, rootHd +: rootTl) if (thisHd == rootHd) =>
            // pop common ancestor
            aux(thisTl, rootTl)
          case _ =>
            // move to inner-most common ancestor and append remaining path
            rootDirectories.map(_ => "..") ++ thisDirectories :+ thisFile
        }
      Path(aux(), thisAnchor)
    }

    def fileUrl: String = file.mkString("/")

    def anchorUrl: String = anchor.mkString("~")

    def url: String = s"$fileUrl.html#$anchorUrl"

  }

}
