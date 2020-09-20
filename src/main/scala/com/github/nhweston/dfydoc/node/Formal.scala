package com.github.nhweston.dfydoc.node

import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class Formal(
  name: Option[String],
  typ: String,
) {

  lazy val toHtml: Node =
    name match {
      case Some(name) => Text(s"$name: $typ")
      case None => Text(typ)
    }

}

object Formal {

  implicit lazy val fmtFormal = Json.format[Formal]

}
