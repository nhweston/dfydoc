package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.{Resolver, Util}
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class ValueParam(
  name: String,
  token: Token,
  typ: TypeRef,
  override val doc: Option[String],
) extends Decl {

  def toHtml(implicit ctx: Resolver): Node =
    if (name.isEmpty) typ.toHtml
    else  <span>{name}: {typ.toHtml}</span>

}

object ValueParam {

  def toHtml(
    vps: Seq[ValueParam],
    parent: Decl,
  )(implicit ctx: Resolver): Node =
    vps match {
      case Nil =>
        Text("()")
      case vps =>
        <span>{
          Util.intersperse(
            vps.map(_.toHtml),
            Text("("),
            Text(", "),
            Text(")"),
          )
        }</span>
    }

  implicit lazy val fmtFormal = Json.format[ValueParam]

}
