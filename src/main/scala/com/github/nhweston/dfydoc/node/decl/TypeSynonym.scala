package com.github.nhweston.dfydoc.node.decl

import com.github.nhweston.dfydoc.Resolver
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

  override def toHtml(implicit ctx: Resolver): Node = {
    val _kws = Text("type")
    val _name = <b>{name}</b>
    val _tparams = TypeParam.toHtml(tparams)
    val _rhs =
      rhs match {
        case None => Text("")
        case Some(rhs) => Text(" = " + rhs)
      }
    <div class="member">
      <p>{_kws} {_name}{_tparams}{_rhs}</p>
      {_doc}
    </div>
  }

}

object TypeSynonym {

  implicit lazy val fmt = Json.format[TypeSynonym]

}
