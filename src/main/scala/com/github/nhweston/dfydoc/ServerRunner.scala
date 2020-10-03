package com.github.nhweston.dfydoc

import java.io.{BufferedOutputStream, BufferedReader, InputStreamReader}
import java.util.Base64

import com.github.nhweston.dfydoc.ServerRunner._
import play.api.libs.json.{JsError, JsNull, JsSuccess, JsValue, Json, Reads}

import scala.annotation.tailrec

case class ServerRunner(
  serverPath: String,
  verbose: Boolean,
) {

  lazy val proc = new ProcessBuilder(serverPath).start()
  lazy val in = new BufferedReader(new InputStreamReader(proc.getInputStream))
  lazy val out = new BufferedOutputStream(proc.getOutputStream)

  @tailrec
  final def read(result: Option[JsValue] = None): JsValue = {
    val line = in.readLine()
    if (verbose && line != null) println(line)
    line match {
      case null => read(result)
      case Eom() => result.getOrElse(JsNull)
      case DocTree(doctree) => read(Some(Json.parse(doctree)))
      case _ => read(result)
    }
  }

  def getJson(path: String): JsValue = {
    val fileName = path.split('/').last
    val json = s"""{"args":[],"filename":"$fileName","source":"$path","sourceIsFile":true}"""
    val enc = Base64.getEncoder.encodeToString(json.getBytes)
    out.write(s"doctree\n$enc\n[[DAFNY-CLIENT: EOM]]\n".getBytes)
    out.flush()
    read()
  }

  def getTree(path: String): Seq[DocNode] =
    Reads.seq[DocNode].reads(getJson(path)) match {
      case JsSuccess(tree, _) => tree
      case e @ JsError(_) =>
        throw new Exception(Util.pprintJson(JsError.toJson(e)))
    }

  def close(): Unit =
    if (proc.isAlive) {
      out.write("quit\n".getBytes)
      out.flush()
    }

  override def finalize(): Unit = {
    close()
    super.finalize()
  }

}

object ServerRunner {

  val Eom = """.*\[\[DAFNY-SERVER: EOM\]\]""".r
  val DocTree = """DOCTREE_START (.*) DOCTREE_END""".r

}
