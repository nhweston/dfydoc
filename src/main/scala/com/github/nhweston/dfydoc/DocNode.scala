package com.github.nhweston.dfydoc

import java.io.{BufferedOutputStream, File, FileOutputStream}

import com.github.nhweston.dfydoc.Resolver.Path
import com.github.nhweston.dfydoc.Util._
import play.api.libs.json.JsonNaming.PascalCase
import play.api.libs.json._

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Attribute, Elem, Null, Text, TopScope, XML, Node => XmlNode}

/** Marker trait for all doc nodes. */
sealed trait DocNode

object DocNode {

  /** A node identified by a token. */
  sealed trait Tokened extends DocNode {

    /** The name of this node. */
    def name: String

    /** Uniquely identifies this node and specifies its containing file and location. */
    def token: Token

    /** All token-identified nodes contained by this node. */
    def children: Seq[Tokened] = Seq.empty

    override def equals(o: Any): Boolean =
      o match {
        case other: Decl => this.token == other.token
        case _ => false
      }

    override def hashCode: Int = token.hashCode

    // convenience methods

    def path(implicit ctx: Resolver): Path =
      ctx.tokensToPaths(token)

    def pathRelativeTo(root: Tokened)(implicit ctx: Resolver): Path =
      path.relativize(root.path)

  }

  /** A program source file or directory. */
  sealed trait Source {

    /** The path to this source from the root. */
    def path: Seq[String]

    /** Writes the output documentation files for this source. */
    def write()(implicit ctx: Resolver): Unit

  }

  /** A node that may have user-written documentation. */
  sealed trait Documentable extends Tokened {

    def identifierToHtml(identifier: String)(implicit ctx: Resolver): XmlNode =
      ctx.resolveLink(this, identifier.split('.').toSeq) match {
        case Some(target) =>
          <span class="identifier"><a href={target.path.url}>{identifier}</a></span>
        case None =>
          <span class="identifier">{identifier}</span>
      }

    /** User-written documentation, if any. */
    def userDoc: Option[String]

    /** User-written documentation converted to HTML. */
    def userDocHtml(implicit ctx: Resolver): XmlNode =
      userDoc.map(markdownToHtml) match {
        case Some(Right(result)) =>
          // success, replace targets in links
          val raw = XML.loadString(s"<div>$result</div>")
          val rewriter = new RewriteRule {
            override def transform(n: XmlNode): Seq[XmlNode] =
              n match {
                case e @ Elem(_, "a", attributes, _, children @_*) =>
                  // element is a link
                  val identifier = e \@ "href"
                  ctx.resolveLink(Documentable.this, identifier.split('.').toSeq) match {
                    case Some(target) =>
                      val url = target.pathRelativeTo(Documentable.this).url
                      val attribute = Attribute("href", Seq(Text(url)), Null)
                      Elem(null, "a", attributes.append(attribute), TopScope, true, children :_*)
                    case None => e
                  }
                case n => n
              }
          }
          val out = (new RuleTransformer(rewriter))(raw).child
          <div>{out}</div>
        case Some(Left(e)) =>
          // failed to convert markdown
          <div><b>Malformed markdown</b>: {Text(e.message)}</div>
        case None =>
          // no user-written documentation
          Text("")
      }

  }

  /** A node that may appear as subdeclaration of a module. */
  sealed trait Decl extends Documentable {

    /** Generates the HTML output. */
    def toHtml(implicit ctx: Resolver): XmlNode

  }

  /** Represents a source file. */
  case class SourceFile(
    path: Seq[String],
    decls: Seq[Decl],
    includes: Seq[Seq[String]],
  ) extends Source with Tokened {

    override def token: Token = Token(path, 0, 0)

    override def name: String = path.last

    override def children: Seq[Tokened] = decls

    override def write()(implicit ctx: Resolver): Unit = {
      val file = new File(ctx.pathOut, path.mkString("", File.separator, ".html"))
      val bos = new BufferedOutputStream(new FileOutputStream(file))
      bos.write("<!DOCTYPE html>\n".getBytes)
      bos.write(html.toString.getBytes)
      bos.close()
    }

    def html(implicit ctx: Resolver): XmlNode = {
      val cssPath =
        if (path.size > 1) Seq.fill(path.size - 1)("..").mkString("", "/", "/styles.css")
        else "styles.css"
      <html>
        <head>
          <title>{name}</title>
          <link rel="stylesheet" href={cssPath}/>
        </head>
        <body>
          <div id="content">
            <h1>{name}</h1>
            {decls.map(_.toHtml)}
          </div>
        </body>
      </html>
    }

  }

  /** Represents a source directory. */
  case class SourceDirectory(
    path: Seq[String],
    contents: Map[String, Source],
  ) extends Source {

    lazy val (files, subdirectories) = {
      contents.values.toSeq.foldLeft((Seq.empty[SourceFile], Seq.empty[SourceDirectory])) {
        case ((fs, ds), f: SourceFile) => (fs :+ f, ds)
        case ((fs, ds), d: SourceDirectory) => (fs, ds :+ d)
      }
    }

    def name: String = path.last

    def index(implicit ctx: Resolver): XmlNode = {
      val title =
        path match {
          case _ :+ title => title
          case Nil => ctx.projectName
        }
      val cssPath =
        if (path.isEmpty) "styles.css"
        else Seq.fill(path.size)("..").mkString("", "/", "/styles.css")
      val subdirectoriesHtml = {
        val heading =
          if (path.isEmpty) "Directories"
          else "Subdirectories"
        subdirectories match {
          case Nil =>
            Text("")
          case subdirectories =>
            <h2>{heading}</h2>
            <ul>{
              subdirectories.map { subdirectory =>
                <li><a href={s"${subdirectory.name}/index.html"}>{subdirectory.name}</a></li>
              }
            }</ul>
        }
      }
      val filesHtml =
        files match {
          case Nil =>
            Text("")
          case files =>
            <h2>Files</h2>
            <ul>{
              files.map { file =>
                <li><a href={s"${file.name}.html"}>{file.name}</a></li>
              }
            }</ul>
        }
      <html>
        <head>
          <title>{title}</title>
          <link rel="stylesheet" href={cssPath}/>
        </head>
        <body>
          <h1>{title}</h1>
          {subdirectoriesHtml}
          {filesHtml}
        </body>
      </html>
    }

    override def write()(implicit ctx: Resolver): Unit = {
      val directory =
        path match {
          case Nil => new File(ctx.pathOut)
          case _ => new File(ctx.pathOut, path.mkString(File.separator))
        }
      directory.mkdir()
      val indexFile = new File(directory, "index.html")
      val bos = new BufferedOutputStream(new FileOutputStream(indexFile))
      bos.write("<!DOCTYPE html>\n".getBytes)
      bos.write(index.toString.getBytes)
      bos.close()
      contents.values.foreach(_.write())
    }

  }

  case class Module(
    name: String,
    isAbstract: Boolean,
    refines: Option[String],  // TODO: use TypeRef
    decls: Seq[Decl],
    userDoc: Option[String],
    token: Token,
  ) extends Decl with Documentable {

    override def children: Seq[Tokened] = decls

    override def toHtml(implicit ctx: Resolver): XmlNode = {
      val keywordsHtml =
        <span class="keyword">{if (isAbstract) "abstract module" else "module"}</span>
      val nameHtml = <span class="name">{name}</span>
      val refinesHtml =
        refines match {
          case Some(identifier) =>
            identifierToHtml(identifier)
          case None =>
            Text("")
        }
      <div class="decl">
        <a name={path.anchorUrl}></a>
        <p>{keywordsHtml} {nameHtml}{refinesHtml}</p>
        {userDocHtml}
        {decls.collect { case decl: Documentable => decl.toHtml }}
      </div>
    }

  }

  case class Import(
    name: String,
    target: Token,
    isOpened: Boolean,
    token: Token,
  ) extends Decl {

    override def userDoc: Option[String] = None

    override def toHtml(implicit ctx: Resolver): XmlNode = Text("")

  }

  case class Class(
    name: String,
    isTrait: Boolean,
    typeParams: Seq[TypeParam],
    `extends`: Seq[String],  // TODO: use TypeRef
    members: Seq[Decl],
    userDoc: Option[String],
    token: Token,
  ) extends Decl with Documentable {

    override lazy val children: Seq[Tokened] = members ++ typeParams

    override def toHtml(implicit ctx: Resolver): XmlNode = {
      val keywordHtml = <span class="keyword">{if (isTrait) "trait" else "class"}</span>
      val nameHtml = <span class="name">{name}</span>
      val typeParamsHtml = TypeParam.toHtml(typeParams)
      val extendsHtml =
        `extends` match {
          case Nil => Text("")
          case identifiers =>
            <br/> ++ intersperse(
              identifiers.map(identifierToHtml),
              <span class="keyword">extends </span>,
              Text(", "),
              Text(""),
            )
        }
      val typeParamsUserDocHtml = TypeParam.userDocHtml(typeParams)
      <div class="decl">
        <a name={path.anchorUrl}/>
        <p>{keywordHtml} {nameHtml}{typeParamsHtml}{extendsHtml}</p>
        {userDocHtml}
        {typeParamsUserDocHtml}
        {members.map(_.toHtml)}
      </div>
    }

  }

  case class Datatype(
    name: String,
    isCodata: Boolean,
    typeParams: Seq[TypeParam],
    ctors: Seq[Ctor],
    userDoc: Option[String],
    token: Token,
  ) extends Decl with Documentable {

    override lazy val children: Seq[Tokened] = ctors ++ typeParams

    override def toHtml(implicit ctx: Resolver): XmlNode = {
      val keywordHtml = <span class="keyword">{if (isCodata) "codatatype" else "datatype"}</span>
      val nameHtml = <span class="name">{name}</span>
      val typeParamsHtml = TypeParam.toHtml(typeParams)
      val ctorsHtml = Ctor.toHtml(ctors)
      <div class="decl">
        <a name={path.anchorUrl}/>
        <p>{keywordHtml} {nameHtml}{typeParamsHtml}</p>
        {userDocHtml}
        {ctorsHtml}
      </div>
    }

  }

  case class TypeSynonym(
    name: String,
    typeParams: Seq[TypeParam],
    rhs: Option[String],
    userDoc: Option[String],
    token: Token,
  ) extends Decl {

    override def children: Seq[Tokened] = typeParams

    override def toHtml(implicit ctx: Resolver): XmlNode = {
      val keywordHtml = <span class="keyword">type</span>
      val nameHtml = <span class="identifier">{name}</span>
      val typeParamsHtml = TypeParam.toHtml(typeParams)
      val rhsHtml =
        rhs match {
          case Some(rhs) => <span class="code"> = {rhs}</span>
          case None => Text("")
        }
      <div class="decl">
        <a name={path.anchorUrl}/>
        <p>{keywordHtml} {nameHtml}{typeParamsHtml}{rhsHtml}</p>
        {userDocHtml}
      </div>
    }

  }

  case class Newtype(
    name: String,
    baseType: TypeRef,
    constraint: String,
    userDoc: Option[String],
    token: Token,
  ) extends Decl {

    override def children: Seq[Tokened] = Seq(baseType)

    override def toHtml(implicit ctx: Resolver): XmlNode = {
      val keywordHtml = <span class="keyword">newtype</span>
      val nameHtml = <span class="identifier">{name}</span>
      val baseTypeHtml = baseType.toHtml
      val constraintHtml = <span class="code">{constraint}</span>;
      <div class="decl">
        <a name={path.anchorUrl}/>
        <p>{keywordHtml} {nameHtml} = {baseTypeHtml} | {constraintHtml}</p>
        {userDocHtml}
      </div>
    }

  }

  case class Function(
    name: String,
    kind: FunctionKind,
    modifiers: Seq[Modifier],
    typeParams: Seq[TypeParam],
    valueParams: Seq[ValueParam],
    returnType: TypeRef,
    spec: Seq[Spec],
    userDoc: Option[String],
    token: Token,
  ) extends Decl {

    override lazy val children: Seq[Tokened] =
      typeParams ++ valueParams

    override def toHtml(implicit ctx: Resolver): XmlNode = {
      val keywords = (modifiers :+ kind).mkString(" ")
      val keywordsHtml = <span class="keyword">{keywords}</span>
      val nameHtml = <span class="identifier">{name}</span>
      val typeParamsHtml = TypeParam.toHtml(typeParams)
      val valueParamsHtml = ValueParam.toHtml(valueParams)
      val returnTypeHtml = returnType.toHtml
      <div class="decl">
        <a name={path.anchorUrl}/>
        <p>{keywordsHtml} {nameHtml}{typeParamsHtml}{valueParamsHtml}: {returnTypeHtml}</p>
        {userDocHtml}
      </div>
    }

  }

  case class Method(
    name: String,
    kind: MethodKind,
    modifiers: Seq[Modifier],
    typeParams: Seq[TypeParam],
    valueParams: Seq[ValueParam],
    returns: Seq[ValueParam],
    spec: Seq[Spec],
    userDoc: Option[String],
    token: Token,
  ) extends Decl {

    override lazy val children: Seq[Tokened] =
      typeParams ++ valueParams ++ returns

    override def toHtml(implicit ctx: Resolver): XmlNode = {
      val keywords = (modifiers :+ kind).mkString(" ")
      val keywordsHtml = <span class="keyword">{keywords}</span>
      val nameHtml = <span class="identifier">{name}</span>
      val typeParamsHtml = TypeParam.toHtml(typeParams)
      val valueParamsHtml = ValueParam.toHtml(valueParams)
      val returnsHtml =
        returns match {
          case Nil => Text("")
          case returns => <span>returns {ValueParam.toHtml(returns)}</span>
        }
      <div class="decl">
        <a name={path.anchorUrl}/>
        <p>{keywordsHtml} {nameHtml}{typeParamsHtml}{valueParamsHtml}<br/>{returnsHtml}</p>
        {userDocHtml}
      </div>
    }

  }

  case class Field(
    name: String,
    `type`: TypeRef,
    userDoc: Option[String],
    token: Token,
  ) extends Decl {

    override def toHtml(implicit ctx: Resolver): XmlNode = {
      val keywordHtml = <span class="keyword">var</span>
      val nameHtml = <span class="identifier">{name}</span>
      val typeHtml = `type`.toHtml
      <div class="decl">
        <a name={path.anchorUrl}/>
        <p>{keywordHtml} {nameHtml}: {typeHtml}</p>
        {userDocHtml}
      </div>
    }

  }

  case class Ctor(
    name: String,
    valueParams: Seq[ValueParam],
    token: Token,
  ) extends Documentable {

    // documentation of constructors currently not supported
    override def userDoc: Option[String] = None

    override def children: Seq[Tokened] = valueParams

    def toHtml(implicit ctx: Resolver): XmlNode = {
      val nameHtml = <span class="identifier">{name}</span>
      val valueParamsHtml = ValueParam.toHtml(valueParams)
      <li>{nameHtml}{valueParamsHtml}</li>
    }

  }

  object Ctor {

    def toHtml(ctors: Seq[Ctor])(implicit ctx: Resolver): XmlNode =
      <ul>{ctors.map(_.toHtml)}</ul>

  }

  case class TypeParam(
    name: String,
    userDoc: Option[String],
    token: Token,
  ) extends Documentable {

    def toHtml: XmlNode =
      <span class="identifier">{name}</span>

    override def userDocHtml(implicit ctx: Resolver): XmlNode =
      userDocHtml match {
        case Elem(_, "div", _, _, children @_*) =>
          <li><span class="identifier">{name}</span>: {children}</li>
        case _ =>
          <li><span class="identifier">{name}</span></li>
      }

  }

  object TypeParam {

    def toHtml(typeParams: Seq[TypeParam])(implicit ctx: Resolver): XmlNode =
      typeParams match {
        case Nil =>
          Text("")
        case typeParams =>
          <span>{
            Util.intersperse(
              typeParams.map(_.toHtml),
              Text("<"),
              Text(", "),
              Text(">"),
            )
          }</span>
      }

    def userDocHtml(typeParams: Seq[TypeParam])(implicit ctx: Resolver): XmlNode =
      typeParams match {
        case Nil =>
          Text("")
        case typeParams =>
          <div>
            <p><b>Type parameters:</b></p>
            <ul>{typeParams.map(_.userDocHtml)}</ul>
          </div>
      }

  }

  case class ValueParam(
    name: String,
    `type`: TypeRef,
    userDoc: Option[String],
    token: Token,
  ) extends Documentable {

    def toHtml(implicit ctx: Resolver): XmlNode = {
      val nameHtml = <span class="identifier">{name}</span>
      val typeHtml = `type`.toHtml
      <span>{nameHtml}: {typeHtml}</span>
    }

    override def userDocHtml(implicit ctx: Resolver): XmlNode =
      userDocHtml match {
        case Elem(_, "div", _, _, children @_*) =>
          <li><span class="identifier">{name}</span>: {children}</li>
        case _ =>
          <li><span class="identifier">{name}</span></li>
      }

  }

  object ValueParam {

    def toHtml(valueParams: Seq[ValueParam])(implicit ctx: Resolver): XmlNode =
      <span>{
        Util.intersperse(
          valueParams.map(_.toHtml),
          Text("("),
          Text(", "),
          Text(")"),
        )
      }</span>

  }

  /**
   * Represents a specification clause.
   *
   * @note Inheritance from `Documentable` is somewhat of a hack. It needs to be `Documentable` so
   * that links can be resolved correctly, but specification clauses are not identified by a token.
   * The `token` field is actually the token of the containing function or method. The resulting
   * behaviour is appropriate – the scope from which links are resolved is that of the containing
   * function or method. This has no complications on the resolver maps so long as `Spec` nodes are
   * excluded from the `children` fields.
   */
  case class Spec(
    kind: String,
    clause: String,
    userDoc: Option[String],
    token: Token,  // HACK: refers to containing node
  ) extends Documentable {

    override def name: String = ""

  }

  case class TypeRef(
    name: String,
    target: Option[Token],
    typeParams: Seq[TypeRef],
    special: Option[SpecialTypeRefKind],
    token: Token,  // HACK: refers to containing node
  ) extends Tokened {

    def toHtml(implicit ctx: Resolver): XmlNode = {
      special match {
        case Some(SpecialTypeRefKind.Tuple) =>
          <span>{
            Util.intersperse(
              typeParams.map(_.toHtml),
              Text("("),
              Text(", "),
              Text(")"),
            )
          }</span>
        case Some(SpecialTypeRefKind.Function) =>
          typeParams match {
            case in +: out +: Nil =>
              <span>{in.toHtml} → {out.toHtml}</span>
            case in :+ out =>
              <span>{
                Util.intersperse(
                  in.map(_.toHtml),
                  Text("("),
                  Text(", "),
                  Text(")"),
                )
              } → {out.toHtml}</span>
            case Nil =>
              throw new MatchError(Nil)
          }
        case _ =>
          val nameHtml =
            target.flatMap(ctx.tokensToNodes.get) match {
              case Some(target) =>
                val url = target.pathRelativeTo(this).url
                <span class="identifier"><a href={url}>{name}</a></span>
              case None =>
                <span class="identifier">{name}</span>
            }
          val typeParamsHtml =
            typeParams match {
              case Nil => Text("")
              case typeParams =>
                <span>{
                  Util.intersperse(
                    typeParams.map(_.toHtml),
                    Text("<"),
                    Text(", "),
                    Text(">"),
                  )
                }</span>
            }
          <span>{nameHtml}{typeParamsHtml}</span>
      }
    }

  }

  /** Uniquely identifies a doc node. */
  case class Token(
    path: Seq[String],
    line: Int,
    column: Int,
  )

  type FunctionKind = FunctionKind.Value

  object FunctionKind extends Enumeration {

    val function = Value
    val `function method` = Value
    val predicate = Value
    val `predicate method` = Value
    val `greatest predicate` = Value
    val `least predicate` = Value

  }

  type MethodKind = MethodKind.Value

  object MethodKind extends Enumeration {

    val method = Value
    val lemma = Value
    val `twostate lemma` = Value
    val `greatest lemma` = Value
    val `least lemma` = Value
    val constructor = Value

  }

  type Modifier = Modifier.Value

  object Modifier extends Enumeration {

    val `abstract` = Value
    val ghost = Value
    val static = Value
    val `protected` = Value

  }

  type SpecialTypeRefKind = SpecialTypeRefKind.Value

  object SpecialTypeRefKind extends Enumeration {

    val Tuple = Value
    val Function = Value

  }

  implicit lazy val config = JsonConfiguration(PascalCase)

  implicit lazy val fmtDecl: Format[Decl] =
    new OFormat[Decl]() {
      case class Aux(__type: String)
      val readsAux = Json.reads[Aux]
      override def reads(json: JsValue): JsResult[Decl] =
        readsAux.reads(json).flatMap { case Aux(kind) =>
          (kind.split(':').head match {
            case "ModuleNode" => fmtModule
            case "ImportNode" => fmtImport
            case "ClassNode" => fmtClass
            case "DatatypeNode" => fmtDatatype
            case "TypeSynonymNode" => fmtTypeSynonym
            case "NewtypeNode" => fmtNewtype
            case "FunctionNode" => fmtFunction
            case "MethodNode" => fmtMethod
            case "FieldNode" => fmtField
          }).reads(json)
        }
      override def writes(o: Decl): JsObject =
        o match {
          case i: Import => fmtImport.writes(i)
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

  implicit lazy val fmtSourceFile: Format[SourceFile] =
    new OFormat[SourceFile] {
      case class Aux(
        path: String,
        decls: Seq[Decl],
        includes: Seq[Seq[String]],
      )
      val fmtAux = Json.format[Aux]
      override def reads(json: JsValue): JsResult[SourceFile] =
        fmtAux.reads(json).map { case Aux(pathAux, decls, includes) =>
          val path = pathAux.split(File.separatorChar).toSeq
          SourceFile(path, decls, includes)
        }
      override def writes(o: SourceFile): JsObject = {
        val SourceFile(path, decls, includes) = o
        val pathAux = path.mkString(File.separator)
        fmtAux.writes(Aux(pathAux, decls, includes))
      }
    }

  implicit lazy val fmtFunctionKind = Json.formatEnum(FunctionKind)
  implicit lazy val fmtMethodKind = Json.formatEnum(MethodKind)
  implicit lazy val fmtModifier = Json.formatEnum(Modifier)
  implicit lazy val fmtSpecialTypeRefKind = Json.formatEnum(SpecialTypeRefKind)

  implicit lazy val fmtToken = Json.format[Token]
  implicit lazy val fmtImport = Json.format[Import]
  implicit lazy val fmtTypeRef = Json.format[TypeRef]
  implicit lazy val fmtTypeParam = Json.format[TypeParam]
  implicit lazy val fmtValueParam = Json.format[ValueParam]
  implicit lazy val fmtModule = Json.format[Module]
  implicit lazy val fmtClass = Json.format[Class]
  implicit lazy val fmtCtor = Json.format[Ctor]
  implicit lazy val fmtDatatype = Json.format[Datatype]
  implicit lazy val fmtTypeSynonym = Json.format[TypeSynonym]
  implicit lazy val fmtNewtype = Json.format[Newtype]
  implicit lazy val fmtSpec = Json.format[Spec]
  implicit lazy val fmtFunction = Json.format[Function]
  implicit lazy val fmtMethod = Json.format[Method]
  implicit lazy val fmtField = Json.format[Field]

}
