package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.Resolver
import com.github.nhweston.dfydoc.Resolver.DeclPath
import com.github.nhweston.dfydoc.Util._
import com.github.nhweston.dfydoc.node.decl._
import play.api.libs.json._

import scala.xml._
import scala.xml.transform.{RewriteRule, RuleTransformer}

trait Decl {

  /** The name of this declaration to be used in anchor paths. */
  def name: String

  /** User-written documentation, if any. */
  def doc: Option[String] = None

  /** All declarations contained within this nodes. */
  def children: Seq[Decl] = Seq.empty

  /** Uniquely identifies this node and specifies its containing file. */
  def token: Token

  /** The absolute node to this entity. */
  def path(implicit ctx: Resolver): DeclPath =
    ctx.tokensToPaths(token)

  def rootFile(implicit ctx: Resolver): SrcFile =
    ctx.files.find(_.path == token.file).get

  def anchor(implicit ctx: Resolver): Seq[String] =
    ctx.tokensToPaths(token).anchor

  def parent(implicit ctx: Resolver): Option[Decl] =
    ctx.parent(token)

  /** The path to this node relative to `root`. */
  def pathRelativeTo(root: Decl)(implicit ctx: Resolver): DeclPath =
    this.path.relativise(root.path)

  /** User-written documentation rendered as HTML. */
  def docHtml(implicit ctx: Resolver): Node =
    doc.map(md2html) match {
      case Some(Right(result)) =>
        val raw = XML.loadString(s"<div>$result</div>")
        val rewriter = new RewriteRule {
          override def transform(n: Node): Seq[Node] =
            n match {
              case e @ Elem(_, "a", attribs, _, children @_*) =>
                val ident = e \@ "href"
                ctx.resolveLink(Decl.this, ident.split('.')) match {
                  case Some(token) =>
                    val url = token.pathRelativeTo(Decl.this).toUrl
                    val attrib = Attribute("href", Seq(Text(url)), Null)
                    Elem(null, "a", attribs.append(attrib), TopScope, true, children :_*)
                  case None => e
                }
              case e => e
            }
        }
        val out = (new RuleTransformer(rewriter))(raw).child
        <div>{out}</div>
      case Some(Left(e)) =>
        <div><b>Malformed markdown</b>: {Text(e.message)}</div>
      case None =>
        Text("")
    }

  /** Generates the HTML output for this declaration. */
  def toHtml(implicit ctx: Resolver): Node

  override def equals(o: Any): Boolean =
    this match {
      case other: Decl => this.token == other.token
      case _ => false
    }

  override def hashCode(): Int = token.hashCode

}

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
