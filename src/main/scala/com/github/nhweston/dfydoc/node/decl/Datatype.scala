package com.github.nhweston.dfydoc.node.decl

import com.github.nhweston.dfydoc.Resolver
import com.github.nhweston.dfydoc.node.decl.Datatype.Ctor
import com.github.nhweston.dfydoc.node.{Decl, Resolvable, Token, TypeParam, ValueParam}
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class Datatype(
  name: String,
  token: Token,
  isCodata: Boolean,
  tparams: Seq[TypeParam],
  ctors: Seq[Ctor],
  doc: Option[String],
) extends Decl {

  // TODO: Include type parameters and constructors
  override lazy val children: Seq[Resolvable] = Seq.empty

  override def toHtml(implicit ctx: Resolver): Node = {
    val _kws = Text(if (isCodata) "codatatype" else "datatype")
    val _name = <b>{name}</b>
    val _tparams = TypeParam.toHtml(tparams)
    val _ctors = ctors.map(ctor => <li>{ctor.toHtml(this)}</li>)
    <div class="member">
      <a name={aname}/>
      <p>{_kws} {_name}{_tparams}</p>
      {_doc}
      <ul>{_ctors}</ul>
    </div>
  }

}

object Datatype {

  case class Ctor(
    name: String,
    vparams: Option[Seq[ValueParam]],
  ) {
    def toHtml(parent: Datatype)(implicit ctx: Resolver): Node =
      vparams match {
        case None => Text(name)
        case Some(vparams) => Text(name + ValueParam.toHtml(vparams, parent))
      }
  }

  implicit lazy val fmtCtor = Json.format[Ctor]
  implicit lazy val fmt = Json.format[Datatype]

}
