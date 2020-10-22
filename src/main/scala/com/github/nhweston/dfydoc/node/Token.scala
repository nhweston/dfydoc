package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.Resolver
import play.api.libs.json.Json

case class Token(
  file: String,
  line: Int,
  col: Int,
) {

  override lazy val toString: String = s"$file:$line:$col"

  override lazy val hashCode: Int = toString.hashCode

  override def equals(o: Any): Boolean =
    o match {
      case other: Token => this.toString == other.toString
      case _ => false
    }

  def decl(implicit ctx: Resolver): Decl = ctx.tokensToNodes(this)

}

object Token {

  implicit lazy val fmt = Json.format[Token]

}
