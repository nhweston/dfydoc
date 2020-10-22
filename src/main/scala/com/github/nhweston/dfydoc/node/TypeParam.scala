package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.{Resolver, Util}
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class TypeParam(
  name: String,
  token: Token,
  override val doc: Option[String],
) extends Decl {

  def toHtml(implicit ctx: Resolver): Node =
    Text(name)

}

object TypeParam {

  def toHtml(tps: Seq[TypeParam])(implicit ctx: Resolver): Node =
    tps match {
      case Nil =>
        Text("")
      case tps =>
        <span>{
          Util.intersperse(
            tps.map(_.toHtml),
            Text("<"),
            Text(", "),
            Text(">"),
          )
        }</span>
    }

  implicit lazy val fmtTypeParam = Json.format[TypeParam]

}
