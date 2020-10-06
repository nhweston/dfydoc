package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.node.decl._
import play.api.libs.json._

trait Decl extends HtmlNode with Resolvable

object Decl {

  object Modifier extends Enumeration {
    val `abstract` = Value
    val ghost = Value
    val static = Value
    val `protected` = Value
  }

  type Modifier = Modifier.Value

  implicit lazy val fmtModifier = Json.formatEnum(Modifier)

  implicit lazy val fmt: Format[Decl] =
    new OFormat[Decl]() {
      case class Aux(__type: String)
      val readsAux = Json.reads[Aux]
      override def reads(json: JsValue): JsResult[Decl] =
        readsAux.reads(json) match {
          case JsSuccess(Aux(kind), _) =>
            (kind.split(':').head match {
              case "ClassInfo" => Class.fmt
              case "DatatypeInfo" => Datatype.fmt
              case "FieldInfo" => Field.fmt
              case "FunctionInfo" => Function.fmt
              case "MethodInfo" => Method.fmt
              case "ModuleInfo" => Module.fmt
              case "NewtypeInfo" => Newtype.fmt
              case "TypeSynonymInfo" => TypeSynonym.fmt
            }).reads(json)
          case e @ JsError(_) => e
        }
      override def writes(o: Decl): JsObject =
        o match {
          case c: Class => Class.fmt.writes(c)
          case d: Datatype => Datatype.fmt.writes(d)
          case f: Field => Field.fmt.writes(f)
          case f: Function => Function.fmt.writes(f)
          case m: Method => Method.fmt.writes(m)
          case m: Module => Module.fmt.writes(m)
          case n: Newtype => Newtype.fmt.writes(n)
          case t: TypeSynonym => TypeSynonym.fmt.writes(t)
        }
    }

}
