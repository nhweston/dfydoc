package com.github.nhweston.dfydoc

import java.io.File
import java.nio.file.Paths

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
            val tree = sr.getTree(file)
            val path = Paths.get(file).toRealPath().toFile.getParent
            val root = PathResolver(path, tree).result
            implicit val ctx = Resolver(root)
            root.write(out)

          case Right(Print(file, content, verbose)) =>
            println(file)
            println((new File(file)).getAbsolutePath)
            implicit val sr = ServerRunner(serverPath, verbose)
            content match {
              case Doc =>
                val f = sr.getTree(file).head
                implicit val ctx = Resolver(f)
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

}
