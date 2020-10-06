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
            root.write(out)

          case Right(Print(file, content, verbose)) =>
            println(file)
            println((new File(file)).getAbsolutePath)
            implicit val sr = ServerRunner(serverPath, verbose)
            content match {
              case Doc =>
                val f = sr.getTree(file).head
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

//  def runAll(
//    serverPath: String,
//    inPath: String,
//    outPath: String,
//    opts: Set[ProgramOptions]
//  ): Unit = {
//    val verbose = opts(Verbose)
//    implicit val sr = ServerRunner(serverPath, verbose)
//    SrcDir(new File(inPath), new File(outPath), "root").write()
//  }
//
//  def run(
//    serverPath: String,
//    filePath: String,
//    opts: Set[ProgramOptions],
//  ): Unit = {
//    val verbose = opts(Verbose)
//    val json = ServerRunner(serverPath, filePath, verbose)
//    if (opts(PrintTreeAsJson))
//      println(pprintJson(json))
//    else Reads.seq[DocNode].reads(json) match {
//      case JsSuccess(symbols, _) =>
//        if (opts(PrintTree))
//          pprint.pprintln(symbols)
//        else {
//          val fileName = filePath.split('/').last.split('.').head
//          val file = SrcFile(fileName, symbols)
//          println("<!DOCTYPE html>")
//          println(file.toHtml.toString)
//        }
//      case e @ JsError(_) =>
//        println(pprintJson(JsError.toJson(e)))
//    }
//  }
//
//  def parseOptions(
//    programName: String,
//    opts: Seq[String]
//  ): Either[String, Set[ProgramOptions]] = {
//    @tailrec
//    def aux(
//      result: Set[ProgramOptions] = Set.empty,
//      opts: Seq[String] = opts,
//    ): Either[String, Set[ProgramOptions]] =
//      opts match {
//        case "-j" +: tl => aux(result + PrintTreeAsJson, tl)
//        case "-t" +: tl => aux(result + PrintTree, tl)
//        case "-v" +: tl => aux(result + Verbose, tl)
//        case Nil =>
//          if (result(PrintTree) && result(PrintTreeAsJson))
//            Left("-j and -t are mutually exclusive")
//          else Right(result)
//        case _ => Left(usage(programName))
//      }
//    aux()
//  }

}
