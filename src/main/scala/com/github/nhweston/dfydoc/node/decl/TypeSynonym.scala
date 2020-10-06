package com.github.nhweston.dfydoc.node.decl

import com.github.nhweston.dfydoc.node.{Decl, Token, TypeParam}
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class TypeSynonym(
  name: String,
  token: Token,
  tparams: Seq[TypeParam],
  rhs: Option[String],
  doc: Option[String],
) extends Decl {

  lazy val _kws = Text("type")
  lazy val _name = <b>{name}</b>
  lazy val _tparams = TypeParam.toHtml(tparams)
  lazy val _rhs =
    rhs match {
      case None => Text("")
      case Some(rhs) => Text(" = " + rhs)
    }

  override lazy val toHtml: Node =
    <div class="member">
      <p>{_kws} {_name}{_tparams}{_rhs}</p>
      {_doc}
    </div>

}

object TypeSynonym {

  implicit lazy val fmt = Json.format[TypeSynonym]

}
