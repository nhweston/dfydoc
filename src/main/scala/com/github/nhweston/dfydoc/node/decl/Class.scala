package com.github.nhweston.dfydoc.node.decl

import com.github.nhweston.dfydoc.node.{Decl, Resolvable, Token, TypeParam}
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class Class(
  name: String,
  token: Token,
  isTrait: Boolean,
  tparams: Seq[TypeParam],
  xtnds: Seq[String],
  members: Seq[Decl],
  doc: Option[String],
) extends Decl {

  lazy val _kws = Text(if (isTrait) "trait" else "class")
  lazy val _name = Text(name)
  lazy val _tparams = TypeParam.toHtml(tparams)
  lazy val _xtnds =
    xtnds match {
      case Nil => Text("")
      case xtnds => <br/> ++ Text(xtnds.mkString("extends ", ", ", ""))
    }

  // TODO: Include type parameters
  override lazy val children: Seq[Resolvable] = members

  override lazy val toHtml: Node =
    <div class="sub">
      <p>{_kws} {_name}{_tparams}{_xtnds}</p>
      {_doc}
      {members.map(_.toHtml)}
    </div>

}

object Class {

  implicit lazy val fmt = Json.format[Class]

}
