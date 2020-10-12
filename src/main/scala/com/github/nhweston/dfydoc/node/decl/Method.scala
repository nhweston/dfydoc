package com.github.nhweston.dfydoc.node.decl

import com.github.nhweston.dfydoc.Resolver
import com.github.nhweston.dfydoc.node.Decl.Modifier
import com.github.nhweston.dfydoc.node.decl.Method.MethodKind
import com.github.nhweston.dfydoc.node._
import play.api.libs.json.Json

import scala.xml.{Node, Text}

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

  override def toHtml(implicit ctx: Resolver): Node = {
    val _kws = (modifiers :+ kind).mkString(" ")
    val _name = <b>{name}</b>
    val _tparams = TypeParam.toHtml(tparams)
    val _vparams = ValueParam.toHtml(vparams)
    val _ret =
      returns match {
        case Nil => Text("")
        case returns => <span>returns {ValueParam.toHtml(returns)}</span>
      }
    <div>
      <p>{_kws} {_name}{_tparams}{_vparams}<br/>{_ret}</p>
      {_doc}
    </div>
  }

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
