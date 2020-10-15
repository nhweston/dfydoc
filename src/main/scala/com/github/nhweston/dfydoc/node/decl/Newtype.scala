package com.github.nhweston.dfydoc.node.decl

import com.github.nhweston.dfydoc.Resolver
import com.github.nhweston.dfydoc.node.{Decl, Token}
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class Newtype(
  name: String,
  token: Token,
  btyp: String,
  constraint: String,
  doc: Option[String],
) extends Decl {

  override def toHtml(implicit ctx: Resolver): Node = {
    val _kws = Text("newtype")
    val _name = <b>{name}</b>
    val _btyp = Text(btyp)
    val _constr = Text(constraint)
    <div class="member">
      <a name={aname}/>
      <p>{_kws} {_name} = {_btyp} | {_constr}</p>
      {_doc(this)}
    </div>
  }

}

object Newtype {

  implicit lazy val fmt = Json.format[Newtype]

}
