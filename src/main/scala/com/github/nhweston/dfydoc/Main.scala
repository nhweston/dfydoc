package com.github.nhweston.dfydoc

import com.github.nhweston.dfydoc.DocNode.File
import com.github.nhweston.dfydoc.Util._
import play.api.libs.json.{JsError, JsSuccess, Reads}

import scala.annotation.tailrec

object Main {

  object ProgramOptions extends Enumeration {
    type ProgramOptions = Value
    val PrintTreeAsJson = Value
    val PrintTree = Value
    val Verbose = Value
  }

  import ProgramOptions._

  def main(args: Array[String]): Unit =
    args.toSeq match {
      case programName +: serverPath +: filePath +: tl =>
        parseOptions(programName, tl) match {
          case Right(opts) =>
            run(serverPath, filePath, opts)
          case Left(msg) =>
            println(msg)
        }
      case programName +: _ +: _ =>
        println(usage(programName))
      case x =>
        throw new MatchError(x)
    }

  def run(
    serverPath: String,
    filePath: String,
    opts: Set[ProgramOptions],
  ): Unit = {
    val verbose = opts(Verbose)
    val json = ServerRunner(serverPath, filePath, verbose)
    if (opts(PrintTreeAsJson))
      println(pprintJson(json))
    else Reads.seq[DocNode].reads(json) match {
      case JsSuccess(symbols, _) =>
        if (opts(PrintTree))
          pprint.pprintln(symbols)
        else {
          val fileName = filePath.split('/').last.split('.').head
          val file = File(fileName, symbols)
          println("<!DOCTYPE html>")
          println(file.toHtml.toString)
        }
      case e @ JsError(_) =>
        println(pprintJson(JsError.toJson(e)))
    }
  }

  def parseOptions(
    programName: String,
    opts: Seq[String]
  ): Either[String, Set[ProgramOptions]] = {
    @tailrec
    def aux(
      result: Set[ProgramOptions] = Set.empty,
      opts: Seq[String] = opts,
    ): Either[String, Set[ProgramOptions]] =
      opts match {
        case "-j" +: tl => aux(result + PrintTreeAsJson, tl)
        case "-t" +: tl => aux(result + PrintTree, tl)
        case "-v" +: tl => aux(result + Verbose, tl)
        case Nil =>
          if (result(PrintTree) && result(PrintTreeAsJson))
            Left("-j and -t are mutually exclusive")
          else Right(result)
        case _ => Left(usage(programName))
      }
    aux()
  }

  def usage(programName: String): String =
    s"""Usage: $programName file [options]
       |    -j  print the symbol tree as a JSON
       |    -t  print the symbol tree
       |    -v  verbose mode""".stripMargin

}
