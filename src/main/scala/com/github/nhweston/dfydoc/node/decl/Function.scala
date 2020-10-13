package com.github.nhweston.dfydoc.node.decl

import com.github.nhweston.dfydoc.Resolver
import com.github.nhweston.dfydoc.node.Decl.Modifier
import com.github.nhweston.dfydoc.node.decl.Function.FunctionKind
import com.github.nhweston.dfydoc.node.{Decl, Spec, Token, TypeParam, ValueParam}
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class Function(
  name: String,
  token: Token,
  kind: FunctionKind,
  modifiers: Seq[Modifier],
  tparams: Seq[TypeParam],
  vparams: Seq[ValueParam],
  rtyp: String,
  spec: Seq[Spec],
  doc: Option[String],
) extends Decl {

  override def toHtml(implicit ctx: Resolver): Node = {
    val _kws = (modifiers :+ kind).mkString(" ")
    val _name = <b>{name}</b>
    val _tparams = TypeParam.toHtml(tparams)
    val _vparams = ValueParam.toHtml(vparams, this)
    val _rtyp = Text(rtyp)
    <div class="member">
      <a name={aname}/>
      {_doc}
      <p>{_kws} {_name}{_tparams}{_vparams}: {_rtyp}</p>
    </div>
  }

}

object Function {

  object FunctionKind extends Enumeration {
    val `function` = Value
    val `function method` = Value
    val `predicate` = Value
    val `predicate method` = Value
    val `inductive predicate` = Value
    val `copredicate` = Value
  }

  type FunctionKind = FunctionKind.Value

  implicit lazy val fmtFunctionKind = Json.formatEnum(FunctionKind)
  implicit lazy val fmt = Json.format[Function]

}
