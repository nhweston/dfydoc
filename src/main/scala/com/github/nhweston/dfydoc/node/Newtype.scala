package com.github.nhweston.dfydoc.node

import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class Newtype(
  name: String,
  btyp: String,
  constraint: String,
  doc: Option[String],
) extends Decl {

  val _kws = Text("newtype")
  val _name = <b>{name}</b>
  val _btyp = Text(btyp)
  val _constr = Text(constraint)

  override lazy val toHtml: Node =
    <div class="member">
      <p>{_kws} {_name} = {_btyp} | {_constr}</p>
      {_doc}
    </div>

}

object Newtype {

  implicit lazy val fmtNewtype = Json.format[Newtype]

}
