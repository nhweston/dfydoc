package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.DocNode
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class Class(
  name: String,
  isTrait: Boolean,
  tparams: TypeParams,
  xtnds: Seq[String],
  members: Seq[DocNode],
  doc: Option[String],
) extends Decl {

  lazy val _kws = Text(if (isTrait) "trait" else "class")
  lazy val _name = Text(name)
  lazy val _tparams = tparams.toHtml
  lazy val _xtnds =
    xtnds match {
      case Nil => Text("")
      case xtnds => <br/> ++ Text(xtnds.mkString("extends ", ", ", ""))
    }

  override lazy val toHtml: Node =
    <div class="sub">
      <p>{_kws} {_name}{_tparams}{_xtnds}</p>
      {members.map(_.toHtml)}
    </div>

}

object Class {

  implicit lazy val fmtClass = Json.format[Class]

}
