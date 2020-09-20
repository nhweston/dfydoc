package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.node.Formal._
import play.api.libs.json.{Format, Reads, Writes}

import scala.xml.{Node, Text}

case class ValueParams(seq: Seq[Formal]) {

  lazy val toHtml: Node =
    seq match {
      case Nil => Text("()")
      case seq => Text(seq.map(_.toHtml.text).mkString("(", ", ", ")"))
    }

}

object ValueParams {

  implicit lazy val fmtVParams =
    Format(
      Reads.seq[Formal](fmtFormal).map(ValueParams(_)),
      Writes.seq[Formal](fmtFormal).contramap[ValueParams](_.seq)
    )

}
