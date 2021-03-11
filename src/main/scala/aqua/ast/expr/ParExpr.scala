package aqua.ast.expr

import aqua.ast.{Expr, Gen, Prog}
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.free.Free
import cats.parse.Parser

case class ParExpr[F[_]](point: F[Unit]) extends Expr[F] {

  // TODO: implement par marker
  def program[Alg[_]]: Prog[Alg, Gen] =
    Free.pure[Alg, Gen](Gen("Par"))
}

object ParExpr extends Expr.AndThen(OnExpr, CoalgebraExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[Expr[F]] =
    `par`.lift.map(ParExpr(_))
}
