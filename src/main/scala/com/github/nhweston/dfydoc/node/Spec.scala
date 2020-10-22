package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.Resolver
import com.github.nhweston.dfydoc.node.Spec._
import play.api.libs.json.Json

import scala.xml.Node

case class Spec(
  kind: SpecKind,
  clause: String,
  override val doc: Option[String],
) extends Decl {

  override def name: String = ""

  override def token: Token = Token("", -1, -1)

  override def toHtml(implicit ctx: Resolver): Node = ???

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
