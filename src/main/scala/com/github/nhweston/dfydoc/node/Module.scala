package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.DocNode
import com.github.nhweston.dfydoc.node.Decl.Modifier
import play.api.libs.json.Json

import scala.xml.{Node, Text}

case class Module(
  name: String,
  modifiers: Seq[Modifier],
  refines: Option[String],
  decls: Seq[DocNode],
  doc: Option[String],
) extends Decl {

  lazy val _kws = Text((modifiers :+ "module").mkString(" "))
  lazy val _name = <b>{name}</b>
  lazy val _ref =
    refines match {
      case Some(refines) => <br/> ++ Text("refines " + refines)
      case None => Text("")
    }

  override lazy val toHtml: Node =
    <div class="sub">
      <p>{_kws} {_name}{_ref}</p>
      {_doc}
      {decls.map(_.toHtml)}
    </div>

}

object Module {

  implicit lazy val fmtModule = Json.format[Module]

}
