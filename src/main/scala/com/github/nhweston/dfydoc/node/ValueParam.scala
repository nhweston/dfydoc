package com.github.nhweston.dfydoc.node

import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class ValueParam(
  name: Option[String],
  typ: String,
  doc: Option[String],
) extends DocNode {

  lazy val toHtml: Node =
    name match {
      case Some(name) => Text(s"$name: $typ")
      case None => Text(typ)
    }

}

object ValueParam {

  def toHtml(vps: Seq[ValueParam]): Node =
    vps match {
      case Nil => Text("()")
      case vps => Text(vps.mkString("(", ", ", ")"))
    }

  implicit lazy val fmtFormal = Json.format[ValueParam]

}
