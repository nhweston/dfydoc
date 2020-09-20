package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.node.Datatype.Ctor
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class Datatype(
  name: String,
  isCodata: Boolean,
  tparams: TypeParams,
  ctors: Seq[Ctor],
  doc: Option[String],
) extends Decl {

  val _kws = Text(if (isCodata) "codatatype" else "datatype")
  val _name = <b>{name}</b>
  val _tparams = tparams.toHtml
  val _ctors = ctors.map(ctor => <li>{ctor.toHtml}</li>)

  override lazy val toHtml: Node =
    <div class="member">
      <p>{_kws} {_name}{_tparams}</p>
      <ul>{_ctors}</ul>
    </div>

}

object Datatype {

  case class Ctor(
    name: String,
    vparams: Option[ValueParams]
  ) {
    lazy val toHtml: Node =
      vparams match {
        case None => Text(name)
        case Some(vparams) => Text(name + vparams.toHtml.text)
        case vparams => Text(name + vparams.mkString("(", ", ", ")"))
      }
  }

  implicit lazy val fmtCtor = Json.format[Ctor]
  implicit lazy val fmtDatatype = Json.format[Datatype]

}
