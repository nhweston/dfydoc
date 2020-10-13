package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.{Resolver, Util}
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class ValueParam(
  name: Option[String],
  typ: TypeRef,
  doc: Option[String],
) extends DocNode {

  def toHtml(parent: Resolvable)(implicit ctx: Resolver): Node =
    name match {
      case Some(name) => <span>{name}: {typ.toHtml(parent)}</span>
      case None => typ.toHtml(parent)
    }

}

object ValueParam {

  def toHtml(
    vps: Seq[ValueParam],
    parent: Resolvable,
  )(implicit ctx: Resolver): Node =
    vps match {
      case Nil =>
        Text("()")
      case vps =>
        <span>{
          Util.intersperse(
            vps.map(_.toHtml(parent)),
            Text("("),
            Text(", "),
            Text(")"),
          )
        }</span>
    }

  implicit lazy val fmtFormal = Json.format[ValueParam]

}
