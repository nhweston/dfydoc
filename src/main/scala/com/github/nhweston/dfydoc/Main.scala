package com.github.nhweston.dfydoc

import java.io.File
import java.net.URI
import java.nio.file.{FileSystems, Files, Paths}

import com.github.nhweston.dfydoc.Options._

object Main {

  object ProgramOptions extends Enumeration {
    type ProgramOptions = Value
    val PrintTreeAsJson = Value
    val PrintTree = Value
    val Verbose = Value
  }

  def main(args: Array[String]): Unit =
    args.toSeq match {
      case programName +: serverPath +: tl =>
        Options(programName, tl) match {

          case Right(Generate(file, out, verbose)) =>
            implicit val sr = ServerRunner(serverPath, verbose)
            val path = Paths.get(file).toRealPath().toFile
            val tree = sr.getTree(path.getAbsolutePath)
            implicit val ctx = new Resolver(path.getParent, out, tree)
            ctx.root.write()
            val cssFile = Paths.get(getClass.getClassLoader.getResource("styles.css").toURI)
            Files.copy(cssFile, Paths.get(ctx.pathOut, "styles.css"))

          case Right(Print(file, content, verbose)) =>
            println(file)
            println((new File(file)).getAbsolutePath)
            implicit val sr = ServerRunner(serverPath, verbose)
            content match {
              case Doc =>
                val f = sr.getTree(file).head
                implicit val ctx = new Resolver("", "", Seq(f))
                println("<!DOCTYPE html>")
                println(f.html.toString)
              case DocTree =>
                Printer(new File(file), false).print()
              case DocTreeJson =>
                Printer(new File(file), true).print()
            }

          case Left(msg) =>
            println(msg)

        }
      case x =>
        throw new MatchError(x)
    }

  def copyCssFile()(implicit ctx: Resolver): Unit = {
    val cssFileUri = getClass.getClassLoader.getResource("styles.css").toURI
    val Seq(fsUri, path) = cssFileUri.toString.split('!').toSeq
    val uri = URI.create(fsUri)
    val fs = FileSystems.newFileSystem(uri, new java.util.HashMap[String, String])
    val cssFile = fs.getPath(path)
    Files.copy(cssFile, Paths.get(ctx.pathOut, "styles.css"))
  }

}
