package com.github.nhweston.dfydoc

import java.io.{BufferedOutputStream, BufferedReader, InputStreamReader}
import java.util.Base64

import play.api.libs.json.{JsNull, JsValue, Json}

import scala.annotation.tailrec

/** Runs the Dafny server and collects the symbol tree. */
object ServerRunner {

  val Eom = """.*\[\[DAFNY-SERVER: EOM\]\]""".r
  val DocTree = """DOCTREE_START (.*) DOCTREE_END""".r

  def apply(
    serverPath: String,
    filePath: String,
    verbose: Boolean,
  ): JsValue = {

    val proc = new ProcessBuilder(serverPath).start()
    val in = new BufferedReader(new InputStreamReader(proc.getInputStream))
    val out = new BufferedOutputStream(proc.getOutputStream)

    @tailrec
    def read(result: Option[JsValue] = None): JsValue = {
      val line = in.readLine()
      if (verbose && line != null) println(line)
      line match {
        case null => read(result)
        case Eom() => result.getOrElse(JsNull)
        case DocTree(doctree) => read(Some(Json.parse(doctree)))
        case _ => read(result)
      }
    }

    val fileName = filePath.split('/').last
    val json = s"""{"args":[],"filename":"$fileName","source":"$filePath","sourceIsFile":true}"""
    val enc = Base64.getEncoder.encodeToString(json.getBytes)
    out.write(s"doctree\n$enc\n[[DAFNY-CLIENT: EOM]]\n".getBytes)
    out.flush()
    val result = read()

    if (proc.isAlive) {
      out.write("quit\n".getBytes)
      out.flush()
    }

    result

  }

}
