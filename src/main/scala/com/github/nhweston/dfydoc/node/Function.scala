package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.node.Decl.Modifier
import com.github.nhweston.dfydoc.node.Function._
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class Function(
  name: String,
  kind: FunctionKind,
  modifiers: Seq[Modifier],
  tparams: TypeParams,
  vparams: ValueParams,
  rtyp: String,
  spec: Seq[Spec],
  doc: Option[String],
) extends Decl {

  lazy val _kws = (modifiers :+ kind).mkString(" ")
  lazy val _name = <b>{name}</b>
  lazy val _tparams = tparams.toHtml
  lazy val _vparams = vparams.toHtml
  lazy val _rtyp = Text(rtyp)

  override lazy val toHtml: Node =
    <div class="member">
      <p>{_kws} {_name}{_tparams}{_vparams}: {_rtyp}</p>
    </div>

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
  implicit lazy val fmtFunction = Json.format[Function]

}
