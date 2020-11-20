package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.node.SpecExpRaw.{BinaryKind, QuantifierKind, UnaryKind}

sealed trait SpecExp

object SpecExp {

  def apply(raw: SpecExpRaw): SpecExp = {
    raw match {
      case SpecExpRaw.Term(text) =>
        Term(text)
      case SpecExpRaw.Quantifier(kind, vars, exp) =>
        (kind match {
          case QuantifierKind.Forall => Forall
          case QuantifierKind.Exists => Exists
        })(vars, SpecExp(exp))
      case SpecExpRaw.Predicate(target, args) =>
        Predicate(target, args)
      case SpecExpRaw.Unary(UnaryKind.Not, arg) =>
        Not(SpecExp(arg))
      case SpecExpRaw.Binary(kind, lhs, rhs) =>
        kind match {
          case BinaryKind.And =>
            And(
              Seq(lhs, rhs).map(SpecExp(_)).foldLeft(Seq.empty[SpecExp]) {
                case (sub, And(exps)) => sub ++ exps
                case (sub, exp) => sub :+ exp
              }
            )
          case BinaryKind.Or =>
            Or(
              Seq(lhs, rhs).map(SpecExp(_)).foldLeft(Seq.empty[SpecExp]) {
                case (sub, Or(exps)) => sub ++ exps
                case (sub, exp) => sub :+ exp
              }
            )
          case BinaryKind.Imp =>
            (SpecExp(lhs), SpecExp(rhs)) match {
              case (And(exps0), Imp(And(exps1), rhs0)) =>
                Imp(And(exps0 ++ exps1), rhs0)
              case (And(exps0), Imp(exp, rhs0)) =>
                Imp(And(exps0 :+ exp), rhs0)
              case (exp, Imp(And(exps), rhs0)) =>
                Imp(And(exp +: exps), rhs0)
              case (exp0, Imp(exp1, rhs0)) =>
                Imp(And(Seq(exp0, exp1)), rhs0)
              case (lhs0, rhs0) =>
                Imp(lhs0, rhs0)
            }
          case BinaryKind.Iff =>
            Iff(SpecExp(lhs), SpecExp(rhs))
        }
    }
  }

  case class Term(text: String) extends SpecExp

  case class Predicate(target: Token, args: Seq[String]) extends SpecExp

  case class Forall(
    vars: Seq[String],
    exp: SpecExp,
  ) extends SpecExp

  case class Exists(
    vars: Seq[String],
    exp: SpecExp,
  ) extends SpecExp

  case class Not(exp: SpecExp) extends SpecExp

  case class And(exps: Seq[SpecExp]) extends SpecExp

  case class Or(exps: Seq[SpecExp]) extends SpecExp

  case class Imp(lhs: SpecExp, rhs: SpecExp) extends SpecExp

  case class Iff(lhs: SpecExp, rhs: SpecExp) extends SpecExp

}
