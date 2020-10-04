package com.github.nhweston.dfydoc.node

case class Token(
  file: String,
  line: Int,
  col: Int,
) {

  override lazy val toString: String = s"$file:$line:$col"

}
