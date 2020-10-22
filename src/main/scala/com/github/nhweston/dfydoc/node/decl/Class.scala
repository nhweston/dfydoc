package com.github.nhweston.dfydoc.node.decl

import com.github.nhweston.dfydoc.Resolver
import com.github.nhweston.dfydoc.node.{Decl, Token, TypeParam}
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class Class(
  name: String,
  token: Token,
  isTrait: Boolean,
  tparams: Seq[TypeParam],
  xtnds: Seq[String],
  members: Seq[Decl],
  override val doc: Option[String],
) extends Decl {

  // TODO: Include type parameters
  override lazy val children: Seq[Decl] = members

  override def toHtml(implicit ctx: Resolver): Node = {
    val _kws = Text(if (isTrait) "trait" else "class")
    val _name = Text(name)
    val _tparams = TypeParam.toHtml(tparams)
    val _xtnds =
      xtnds match {
        case Nil => Text("")
        case xtnds => <br/> ++ Text(xtnds.mkString("extends ", ", ", ""))
      }
    <div class="sub">
      <a name={path.getAnchorUrl}/><p>{_kws} {_name}{_tparams}{_xtnds}</p>
      {docHtml}
      {members.map(_.toHtml)}
    </div>
  }

}

object Class {

  implicit lazy val fmt = Json.format[Class]

}
