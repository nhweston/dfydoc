package com.github.nhweston.dfydoc

import scala.annotation.tailrec

sealed trait Options

object Options {

  case class Print(
    file: String,
    content: PrintContent = Doc,
    verbose: Boolean = false,
  ) extends Options

  sealed trait PrintContent
  case object Doc extends PrintContent
  case object DocTree extends PrintContent
  case object DocTreeJson extends PrintContent

  case class Generate(
    inDir: String,
    outDir: String,
    verbose: Boolean = false,
  ) extends Options

  def apply(
    programName: String,
    params: Seq[String],
  ): Either[String, Options] =
    params match {
      case "-g" +: in +: out +: tl =>
        parseGenerate(
          programName,
          Generate(in, out),
          tl,
        )
      case "-p" +: file +: tl =>
        parsePrint(
          programName,
          Print(file),
          tl,
        )
      case _ => Left(usage(programName))
    }

  @tailrec
  def parseGenerate(
    programName: String,
    opts: Generate,
    params: Seq[String],
  ): Either[String, Generate] =
    params match {
      case "-v" +: tl =>
        parseGenerate(
          programName,
          opts.copy(verbose = true),
          tl,
        )
      case Nil => Right(opts)
      case _ => Left(usage(programName))
    }

  @tailrec
  def parsePrint(
    programName: String,
    opts: Print,
    params: Seq[String],
  ): Either[String, Print] = {
    params match {
      case "-j" +: tl =>
        parsePrint(
          programName,
          opts.copy(content = DocTreeJson),
          tl,
        )
      case "-t" +: tl =>
        parsePrint(
          programName,
          opts.copy(content = DocTree),
          tl,
        )
      case "-v" +: tl =>
        parsePrint(
          programName,
          opts.copy(verbose = true),
          tl,
        )
      case Nil => Right(opts)
      case _ => Left(usage(programName))
    }
  }

  def usage(programName: String): String =
    s"""Usage:
       |
       |    Generate documentation for a project:
       |    $programName -g in_dir out_dir [options]
       |        -v  verbose mode
       |
       |    Print documentation for a single file:
       |    $programName -p file [options]
       |        -j  print the doc tree as a JSON
       |        -t  print the doc tree
       |        -v  verbose mode
       |""".stripMargin

}
