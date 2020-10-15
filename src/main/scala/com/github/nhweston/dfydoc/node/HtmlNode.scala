package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.Resolver
import com.github.nhweston.dfydoc.node.HtmlNode.transf
import laika.api.Transformer
import laika.format.{HTML, Markdown}

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml._

trait HtmlNode extends DocNode {

  def doc: Option[String]

  def _doc(parent: Resolvable)(implicit ctx: Resolver): Node =
    doc.map(transf.transform) match {
      case Some(Right(result)) =>
        val raw = XML.loadString("<div>" + result + "</div>")
        val rewriter = new RewriteRule {
          override def transform(n: Node): Seq[Node] =
            n match {
              case e @ Elem(prefix, "a", attribs, scope, children) =>
                val rawLink = e \@ "href"
                ctx.resolveLink(parent, rawLink.split('.')) match {
                  case Some(token) =>
                    val url = ctx.getRelativePath(token, parent.token)
                    println(url)
                    val attrib = Attribute("href", Seq(Text(url)), Null)
                    Elem(prefix, "a", attribs.append(attrib), scope, true, children :_*)
                  case None =>
                    println("Warning: unresolved link")
                    println(e)
                    e
                }
              case e =>
                println("No links in doc comment")
                e
            }
        }
        val transformer = new RuleTransformer(rewriter)
        <div>{transformer(raw).child}</div>
      case Some(Left(e)) =>
        <div><b>Malformed markdown</b>: {Text(e.message)}</div>
      case None =>
        Text("")
    }

  def toHtml(implicit ctx: Resolver): Node

}

object HtmlNode {

  val transf = Transformer.from(Markdown).to(HTML).build

}
