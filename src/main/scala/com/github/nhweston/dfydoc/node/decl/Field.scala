package com.github.nhweston.dfydoc.node.decl

import com.github.nhweston.dfydoc.Resolver
import com.github.nhweston.dfydoc.node.{Decl, Token}
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class Field(
  name: String,
  token: Token,
  typ: String,
  doc: Option[String],
) extends Decl {

  lazy val _kwd = "var"
  lazy val _name = <b>{name}</b>
  lazy val _typ = Text(typ)

  override def toHtml(implicit ctx: Resolver): Node =
    <div>
      <a name={aname}/>
      <p>{_kwd} {_name}: {_typ}</p>
      {_doc(this)}
    </div>

}

object Field {

  implicit lazy val fmt = Json.format[Field]

}
