package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.node.TypeRef._
import com.github.nhweston.dfydoc.{Resolver, Util}
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class TypeRef(
  name: String,
  target: Option[Token] = None,
  tparams: Seq[TypeRef] = Seq.empty,
  special: Option[SpecialTypeRefKind] = None,
) extends Decl {

  override def token: Token = Token("", -1, -1)

  def toHtml(implicit ctx: Resolver): Node =
    special match {

      case Some(SpecialTypeRefKind.Tuple) =>
        <span>{
          Util.intersperse(
            tparams.map(_.toHtml),
            Text("("),
            Text(", "),
            Text(")"),
          )
        }</span>

      case Some(SpecialTypeRefKind.Function) =>
        tparams match {
          case in +: out +: Nil =>
            <span>{in.toHtml} → {out.toHtml}</span>
          case in :+ out =>
            <span>{
              Util.intersperse(
                in.map(_.toHtml),
                Text("("),
                Text(", "),
                Text(")"),
              )
            } → {out.toHtml}</span>
        }

      case None =>
        val _name =
          target match {
            case Some(target) =>
              val rel = target.decl.pathRelativeTo(this)
              println(rel)
              <a href={rel.toUrl}>{name}</a>
            case None =>
              Text(name)
          }
        val _tparams =
          tparams match {
            case Nil => Text("")
            case tparams =>
              <span>{
                Util.intersperse(
                  tparams.map(_.toHtml),
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
