package com.github.nhweston.dfydoc.node

import play.api.libs.json.{Format, Reads, Writes}

import scala.xml.{Node, Text}

case class TypeParams(seq: Seq[String]) {

  lazy val toHtml: Node =
    seq match {
      case Nil => Text("")
      case seq => Text(seq.mkString("<", ", ", ">"))
    }

}

object TypeParams {

  implicit lazy val fmtTypeParams =
    Format(
      Reads.seq[String].map(TypeParams(_)),
      Writes.seq[String].contramap[TypeParams](_.seq)
    )

}
