package com.github.nhweston.dfydoc.node

import play.api.libs.json.Json

case class Token(
  file: String,
  line: Int,
  col: Int,
) extends DocNode {

  override lazy val toString: String = s"$file:$line:$col"

  override lazy val hashCode: Int = toString.hashCode

  override def equals(o: Any): Boolean =
    o match {
      case other: Token => this.toString == other.toString
      case _ => false
    }

}

object Token {

  implicit lazy val fmt = Json.format[Token]

}
