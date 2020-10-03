package com.github.nhweston.dfydoc.node

import java.io.{BufferedOutputStream, File, FileOutputStream}

import com.github.nhweston.dfydoc.{DocNode, ServerRunner}

import scala.xml.Node

case class SrcFile(
  inParent: File,
  outParent: File,
  name: String,
)(implicit sr: ServerRunner) extends DocNode {

  override def doc: Option[String] = None

  lazy val f: File = new File(inParent.getAbsolutePath, name)

  lazy val decls: Seq[DocNode] =
    sr.getTree(f.getAbsolutePath)

  def write(): Unit = {
    val f = new File(outParent.getAbsolutePath, s"$name.html")
    val bos = new BufferedOutputStream(new FileOutputStream(f))
    bos.write("<!DOCTYPE html>".getBytes)
    bos.write(toHtml.toString.getBytes)
    bos.close()
  }

  override lazy val toHtml: Node =
    <html>
      <head>
        <title>{name}</title>
      </head>
      <body>
        <h1>{name}</h1>
        {decls.map(_.toHtml)}
      </body>
    </html>

}
