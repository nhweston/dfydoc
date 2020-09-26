package com.github.nhweston.dfydoc

import com.github.nhweston.dfydoc.DocNode._
import com.github.nhweston.dfydoc.node._
import laika.api.Transformer
import laika.format.{HTML, Markdown}
import play.api.libs.json._

import scala.xml.{Node, Text, XML}

trait DocNode {

  def doc: Option[String]

  lazy val _doc: Node =
    doc.map(transf.transform) match {
      case Some(Right(result)) =>
        <div>{XML.loadString(result)}</div>
      case Some(Left(e)) =>
        <div><b>Malformed markdown</b>: {Text(e.message)}</div>
      case None =>
        Text("")
    }

  def toHtml: Node

}

object DocNode {

  val transf = Transformer.from(Markdown).to(HTML).build

  implicit lazy val fmtDecl: Format[DocNode] =
    new OFormat[DocNode]() {
      case class Aux(__type: String)
      val readsAux = Json.reads[Aux]
      override def reads(json: JsValue): JsResult[DocNode] =
        readsAux.reads(json) match {
          case JsSuccess(Aux(kind), _) =>
            (kind.split(':').head match {
              case "ModuleInfo" => implicitly[OFormat[Module]]
              case "ClassInfo" => implicitly[OFormat[Class]]
              case "DatatypeInfo" => implicitly[OFormat[Datatype]]
              case "TypeSynonymInfo" => implicitly[OFormat[TypeSynonym]]
              case "NewtypeInfo" => implicitly[OFormat[Newtype]]
              case "FunctionInfo" => implicitly[OFormat[Function]]
              case "MethodInfo" => implicitly[OFormat[Method]]
              case "FieldInfo" => implicitly[OFormat[Field]]
            }).reads(json)
          case e @ JsError(_) => e
        }
      override def writes(o: DocNode): JsObject =
        o match {
          case m: Module => implicitly[OFormat[Module]].writes(m)
          case c: Class => implicitly[OFormat[Class]].writes(c)
          case d: Datatype => implicitly[OFormat[Datatype]].writes(d)
          case t: TypeSynonym => implicitly[OFormat[TypeSynonym]].writes(t)
          case n: Newtype => implicitly[OFormat[Newtype]].writes(n)
          case f: Function => implicitly[OFormat[Function]].writes(f)
          case m: Method => implicitly[OFormat[Method]].writes(m)
          case f: Field => implicitly[OFormat[Field]].writes(f)
        }
    }

}
