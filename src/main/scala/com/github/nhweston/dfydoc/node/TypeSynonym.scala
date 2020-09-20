package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.DocNode
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class TypeSynonym(
  name: String,
  tparams: TypeParams,
  rhs: Option[String],
  doc: Option[String],
) extends Decl {

  lazy val _kws = Text("type")
  lazy val _name = <b>{name}</b>
  lazy val _tparams = tparams.toHtml
  lazy val _rhs =
    rhs match {
      case None => Text("")
      case Some(rhs) => Text(" = " + rhs)
    }

  override lazy val toHtml: Node =
    <div class="member">
      <p>{_kws} {_name}{_tparams}{_rhs}</p>
    </div>

}

object TypeSynonym {

  implicit lazy val fmtTypeSynonym = Json.format[TypeSynonym]

}
