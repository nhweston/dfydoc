package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.node.HtmlNode.transf
import laika.api.Transformer
import laika.format.{HTML, Markdown}

import scala.xml.{Node, Text, XML}

trait HtmlNode extends DocNode {

  def doc: Option[String]

  lazy val _doc: Node =
    doc.map(transf.transform) match {
      case Some(Right(result)) =>
        <div>{XML.loadString(result)}</div>
      case Some(Left(e)) =>
        <div><b>Malformed markdown</b>: {Text(e.message)}</div>
      case None =>
        Text("")
    }

  def toHtml: Node

}

object HtmlNode {

  val transf = Transformer.from(Markdown).to(HTML).build

}
