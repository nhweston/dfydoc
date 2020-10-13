package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.node.TypeRef._
import com.github.nhweston.dfydoc.{Resolver, Util}
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class TypeRef(
  name: String,
  token: Option[Token] = None,
  tparams: Seq[TypeRef] = Seq.empty,
  special: Option[SpecialTypeRefKind] = None,
) extends DocNode {

  def toHtml(parent: Resolvable)(implicit ctx: Resolver): Node =
    special match {

      case Some(SpecialTypeRefKind.Tuple) =>
        <span>{
          Util.intersperse(
            tparams.map(_.toHtml(parent)),
            Text("("),
            Text(", "),
            Text(")"),
          )
        }</span>

      case Some(SpecialTypeRefKind.Function) =>
        tparams match {
          case in +: out +: Nil =>
            <span>{in.toHtml(parent)} → {out.toHtml(parent)}</span>
          case in :+ out =>
            <span>{
              Util.intersperse(
                in.map(_.toHtml(parent)),
                Text("("),
                Text(", "),
                Text(")"),
              )
            } → {out.toHtml(parent)}</span>
        }

      case None =>
        val _name =
          token match {
            case Some(target) =>
              val rel = ctx.getRelativePath(target, parent.token)
              <a href={rel}>{name}</a>
            case None =>
              Text(name)
          }
        val _tparams =
          tparams match {
            case Nil => Text("")
            case tparams =>
              <span>{
                Util.intersperse(
                  tparams.map(_.toHtml(parent)),
                  Text("<"),
                  Text(", "),
                  Text(">"),
                )
                }</span>
          }
        <span>{_name}{_tparams}</span>
    }

}

object TypeRef {

  type SpecialTypeRefKind = SpecialTypeRefKind.Value

  object SpecialTypeRefKind extends Enumeration {
    val Tuple = Value
    val Function = Value
  }

  implicit lazy val fmtSpecialTypeRefKind = Json.formatEnum(SpecialTypeRefKind)
  implicit lazy val fmtTypeRef = Json.format[TypeRef]

}
