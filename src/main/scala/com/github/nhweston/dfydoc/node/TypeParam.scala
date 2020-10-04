package com.github.nhweston.dfydoc.node

import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class TypeParam(
  name: String,
  doc: Option[String],
)

object TypeParam {

  def toHtml(tps: Seq[TypeParam]): Node =
    tps match {
      case Nil => Text("")
      case tps => Text(tps.mkString("<", ", ", ">"))
    }

  implicit lazy val fmtTypeParam = Json.format[TypeParam]

}
