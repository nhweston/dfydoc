package com.github.nhweston.dfydoc

import com.github.nhweston.dfydoc.Symbol.FunctionKind.FunctionKind
import com.github.nhweston.dfydoc.Symbol.MethodKind.MethodKind
import com.github.nhweston.dfydoc.Symbol.Modifier.Modifier
import com.github.nhweston.dfydoc.Symbol.SpecKind.SpecKind
import com.github.nhweston.dfydoc.Symbol._
import play.api.libs.json._

import scala.xml.{Node, Text}

sealed trait Symbol {

  lazy val toHtml: Node =
    this match {

      case Module(name, modifiers, refines, decls) =>
        val xKeywords = Text((modifiers :+ "module").mkString(" "))
        val xName = <b>{name}</b>
        val xRefines =
          refines match {
            case Some(refines) => <br/> ++ Text("refines " + refines)
            case None => Text("")
          }
        <div class="sub">
          <p>{xKeywords} {xName}{xRefines}</p>
          {decls.map(_.toHtml)}
        </div>

      case Class(name, isTrait, tparams, xtnds, members) =>
        val xKeyword = Text(if (isTrait) "trait" else "class")
        val xName = Text(name)
        val xTParams = tparams.toHtml
        val xXtnds =
          xtnds match {
            case Nil => Text("")
            case xtnds => <br/> ++ Text(xtnds.mkString("extends ", ", ", ""))
          }
        <div class="sub">
          <p>{xKeyword} {xName}{xTParams}{xXtnds}</p>
          {members.map(_.toHtml)}
        </div>

      case Datatype(name, isCodata, tparams, ctors) =>
        val xKeyword = Text(if (isCodata) "codatatype" else "datatype")
        val xName = <b>{name}</b>
        val xTParams = tparams.toHtml
        val xCtors = ctors.map(ctor => <li>{ctor.toHtml}</li>)
        <div class="member">
          <p>{xKeyword} {xName}{xTParams}</p>
          <ul>{xCtors}</ul>
        </div>

      case TypeSynonym(name, tparams, rhs) =>
        val xKeyword = Text("type")
        val xName = <b>{name}</b>
        val xTParams = tparams.toHtml
        val xRhs =
          rhs match {
            case None => Text("")
            case Some(rhs) => Text(" = " + rhs)
          }
        <div class="member">
          <p>{xKeyword} {xName}{xTParams}{xRhs}</p>
        </div>

      case Newtype(name, btyp, constraint) =>
        val xKeyword = Text("newtype")
        val xName = <b>{name}</b>
        val xBtyp = Text(btyp)
        val xConstraint = Text(constraint)
        <div class="member">
          <p>{xKeyword} {xName} = {xBtyp} | {xConstraint}</p>
        </div>

      case Function(name, kind, modifiers, tparams, vparams, rtyp, spec) =>
        val xKeywords = (modifiers :+ kind).mkString(" ")
        val xName = <b>{name}</b>
        val xTParams = tparams.toHtml
        val xVParams = vparams.toHtml
        val xRTyp = Text(rtyp)
        <div class="member">
          <p>{xKeywords} {xName}{xTParams}{xVParams}: {xRTyp}</p>
        </div>

      case Method(name, kind, modifiers, tparams, vparams, returns, spec) =>
        val xKeywords = (modifiers :+ kind).mkString(" ")
        val xName = <b>{name}</b>
        val xTParams = tparams.toHtml
        val xVParams = vparams.toHtml
        val xReturns = returns.toHtml
        <div>
          <p>{xKeywords} {xName}{xTParams}{xVParams}<br/>returns {xReturns}</p>
        </div>

      case Field(name, typ) =>
        val xKeyword = "var"
        val xName = <b>{name}</b>
        val xTyp = Text(typ)
        <div>
          <p>{xKeyword} {xName}: {xTyp}</p>
        </div>

    }

}

object Symbol {

  case class File(
    name: String,
    decls: Seq[Symbol],
  ) {
    lazy val toHtml: Node =
      <html>
        <head>
          <title>{name}</title>
        </head>
        <body>
          <h1>{name}</h1>
          {decls.map(_.toHtml)}
        </body>
      </html>
  }

  object Modifier extends Enumeration {
    type Modifier = Value
    val `abstract` = Value
    val ghost = Value
    val static = Value
    val `protected` = Value
  }

  object SpecKind extends Enumeration {
    type SpecKind = Value
    val requires = Value
    val ensures = Value
    val modifies = Value
    val reads = Value
    val decreases = Value
  }

  object FunctionKind extends Enumeration {
    type FunctionKind = Value
    val `function` = Value
    val `function method` = Value
    val `predicate` = Value
    val `predicate method` = Value
    val `inductive predicate` = Value
    val `copredicate` = Value
  }

  object MethodKind extends Enumeration {
    type MethodKind = Value
    val `method` = Value
    val `lemma` = Value
    val `colemma` = Value
    val `inductive lemma` = Value
    val `constructor` = Value
  }

  case class TParams(seq: Seq[String]) {
    lazy val toHtml: Node =
      seq match {
        case Nil => Text("")
        case seq => Text(seq.mkString("<", ", ", ">"))
      }
  }

  case class VParams(seq: Seq[Formal]) {
    lazy val toHtml: Node =
      seq match {
        case Nil => Text("()")
        case seq => Text(seq.map(_.toHtml.text).mkString("(", ", ", ")"))
      }
  }

  case class Spec(
    kind: SpecKind,
    clause: String,
    doc: Option[String],
  )

  case class Formal(
    name: Option[String],
    typ: String,
  ) {
    lazy val toHtml: Node =
      name match {
        case Some(name) => Text(s"$name: $typ")
        case None => Text(typ)
      }
  }

  case class Ctor(
    name: String,
    vparams: Option[VParams]
  ) {
    lazy val toHtml: Node =
      vparams match {
        case None => Text(name)
        case Some(vparams) => Text(name + vparams.toHtml.text)
        case vparams => Text(name + vparams.mkString("(", ", ", ")"))
      }
  }

  /////////////
  // MODULES //
  /////////////

  case class Module(
    name: String,
    modifiers: Seq[Modifier],
    refines: Option[String],
    decls: Seq[Symbol],
  ) extends Symbol

  case class Class(
    name: String,
    isTrait: Boolean,
    tparams: TParams,
    xtnds: Seq[String],
    members: Seq[Symbol],
  ) extends Symbol

  ///////////
  // TYPES //
  ///////////

  case class Datatype(
    name: String,
    isCodata: Boolean,
    tparams: TParams,
    ctors: Seq[Ctor],
  ) extends Symbol

  case class TypeSynonym(
    name: String,
    tparams: TParams,
    rhs: Option[String],
  ) extends Symbol

  case class Newtype(
    name: String,
    btyp: String,
    constraint: String,
  ) extends Symbol

  /////////////
  // MEMBERS //
  /////////////

  case class Function(
    name: String,
    kind: FunctionKind,
    modifiers: Seq[Modifier],
    tparams: TParams,
    vparams: VParams,
    rtyp: String,
    spec: Seq[Spec],
  ) extends Symbol

  case class Method(
    name: String,
    kind: MethodKind,
    modifiers: Seq[Modifier],
    tparams: TParams,
    vparams: VParams,
    returns: VParams,
    spec: Seq[Spec],
  ) extends Symbol

  case class Field(
    name: String,
    typ: String,
  ) extends Symbol

  implicit lazy val fmtDecl: Format[Symbol] =
    new OFormat[Symbol]() {
      case class Aux(__type: String)
      val readsAux = Json.reads[Aux]
      override def reads(json: JsValue): JsResult[Symbol] =
        readsAux.reads(json) match {
          case JsSuccess(Aux(kind), _) =>
            (kind.split(':').head match {
              case "ModuleInfo" => fmtModule
              case "ClassInfo" => fmtClass
              case "DatatypeInfo" => fmtDatatype
              case "TypeSynonymInfo" => fmtTypeSynonym
              case "NewtypeInfo" => fmtNewtype
              case "FunctionInfo" => fmtFunction
              case "MethodInfo" => fmtMethod
              case "FieldInfo" => fmtField
            }).reads(json)
          case e @ JsError(_) => e
        }
      override def writes(o: Symbol): JsObject =
        o match {
          case m: Module => fmtModule.writes(m)
          case c: Class => fmtClass.writes(c)
          case d: Datatype => fmtDatatype.writes(d)
          case t: TypeSynonym => fmtTypeSynonym.writes(t)
          case n: Newtype => fmtNewtype.writes(n)
          case f: Function => fmtFunction.writes(f)
          case m: Method => fmtMethod.writes(m)
          case f: Field => fmtField.writes(f)
        }
    }

  implicit lazy val fmtModifier = Json.formatEnum(Modifier)
  implicit lazy val fmtSpecKind = Json.formatEnum(SpecKind)
  implicit lazy val fmtFunctionKind = Json.formatEnum(FunctionKind)
  implicit lazy val fmtMethodKind = Json.formatEnum(MethodKind)

  implicit lazy val fmtTParams =
    Format(
      Reads.seq[String].map(TParams),
      Writes.seq[String].contramap[TParams](_.seq)
    )

  implicit lazy val fmtVParams =
    Format(
      Reads.seq[Formal](fmtFormal).map(VParams),
      Writes.seq[Formal](fmtFormal).contramap[VParams](_.seq)
    )

  implicit lazy val fmtSpec = Json.format[Spec]
  implicit lazy val fmtFormal = Json.format[Formal]
  implicit lazy val fmtCtor = Json.format[Ctor]
  implicit lazy val fmtModule = Json.format[Module]
  implicit lazy val fmtClass = Json.format[Class]
  implicit lazy val fmtDatatype = Json.format[Datatype]
  implicit lazy val fmtTypeSynonym = Json.format[TypeSynonym]
  implicit lazy val fmtNewtype = Json.format[Newtype]
  implicit lazy val fmtFunction = Json.format[Function]
  implicit lazy val fmtMethod = Json.format[Method]
  implicit lazy val fmtField = Json.format[Field]

}
