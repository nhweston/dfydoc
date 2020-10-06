package com.github.nhweston.dfydoc.node.decl

import com.github.nhweston.dfydoc.node.Decl.Modifier
import com.github.nhweston.dfydoc.node.decl.Method.MethodKind
import com.github.nhweston.dfydoc.node.{Decl, Spec, Token, TypeParam, ValueParam}
import play.api.libs.json.Json

import scala.xml.Node

case class Method(
  name: String,
  token: Token,
  kind: MethodKind,
  modifiers: Seq[Modifier],
  tparams: Seq[TypeParam],
  vparams: Seq[ValueParam],
  returns: Seq[ValueParam],
  spec: Seq[Spec],
  doc: Option[String],
) extends Decl {

  lazy val _kws = (modifiers :+ kind).mkString(" ")
  lazy val _name = <b>{name}</b>
  lazy val _tparams = TypeParam.toHtml(tparams)
  lazy val _vparams = ValueParam.toHtml(vparams)
  lazy val _ret = ValueParam.toHtml(returns)

  override lazy val toHtml: Node =
    <div>
      <p>{_kws} {_name}{_tparams}{_vparams}<br/>returns {_ret}</p>
      {_doc}
    </div>

}

object Method {

  object MethodKind extends Enumeration {
    val `method` = Value
    val `lemma` = Value
    val `colemma` = Value
    val `inductive lemma` = Value
    val `constructor` = Value
  }

  type MethodKind = MethodKind.Value

  implicit lazy val fmtMethodKind = Json.formatEnum(MethodKind)
  implicit lazy val fmt = Json.format[Method]

}
