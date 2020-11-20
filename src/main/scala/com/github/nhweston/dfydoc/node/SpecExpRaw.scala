package com.github.nhweston.dfydoc.node

sealed trait SpecExpRaw

object SpecExpRaw {

  case class Term(text: String) extends SpecExpRaw

  case class Quantifier(
    kind: QuantifierKind,
    vars: Seq[String],
    exp: SpecExpRaw,
  ) extends SpecExpRaw

  case class Predicate(
    target: Token,
    args: Seq[String],
  ) extends SpecExpRaw

  case class Unary(
    kind: UnaryKind,
    arg: SpecExpRaw,
  ) extends SpecExpRaw

  case class Binary(
    kind: BinaryKind,
    lhs: SpecExpRaw,
    rhs: SpecExpRaw,
  ) extends SpecExpRaw

  object QuantifierKind extends Enumeration {
    val Forall = Value
    val Exists = Value
  }

  type QuantifierKind = QuantifierKind.Value

  object UnaryKind extends Enumeration {
    val Not = Value
  }

  type UnaryKind = UnaryKind.Value

  object BinaryKind extends Enumeration {
    val And = Value
    val Or = Value
    val Imp = Value
    val Iff = Value
  }

  type BinaryKind = BinaryKind.Value

}
