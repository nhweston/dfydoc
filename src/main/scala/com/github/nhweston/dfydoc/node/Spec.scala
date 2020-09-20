package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.DocNode
import com.github.nhweston.dfydoc.node.Spec._
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class Spec(
  kind: SpecKind,
  clause: String,
  doc: Option[String],
) extends DocNode {

  override lazy val toHtml: Node = ???

}

object Spec {

  object SpecKind extends Enumeration {
    val requires = Value
    val ensures = Value
    val modifies = Value
    val reads = Value
    val decreases = Value
  }

  type SpecKind = SpecKind.Value

  implicit lazy val fmtSpecKind = Json.formatEnum(SpecKind)
  implicit lazy val fmtSpec = Json.format[Spec]

}
