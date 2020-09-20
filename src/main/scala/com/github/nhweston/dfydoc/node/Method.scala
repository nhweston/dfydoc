package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.node.Decl.Modifier
import com.github.nhweston.dfydoc.node.Method.MethodKind
import play.api.libs.json.Json

import scala.xml.Node

case class Method(
  name: String,
  kind: MethodKind,
  modifiers: Seq[Modifier],
  tparams: TypeParams,
  vparams: ValueParams,
  returns: ValueParams,
  spec: Seq[Spec],
  doc: Option[String],
) extends Decl {

  lazy val _kws = (modifiers :+ kind).mkString(" ")
  lazy val _name = <b>{name}</b>
  lazy val _tparams = tparams.toHtml
  lazy val _vparams = vparams.toHtml
  lazy val _ret = returns.toHtml

  override lazy val toHtml: Node =
    <div>
      <p>{_kws} {_name}{_tparams}{_vparams}<br/>returns {_ret}</p>
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
  implicit lazy val fmtMethod = Json.format[Method]

}
