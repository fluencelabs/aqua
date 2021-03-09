package aqua.parser.ast

import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import aqua.parser.lexer.Token._
import LiftParser._
import cats.free.Free

case class ParExpr[F[_]](point: F[Unit]) extends Expr[F] {

  // TODO: implement par marker
  def program[Alg[_]]: Prog[Alg, Unit] =
    Free.pure[Alg, Unit](())
}

object ParExpr extends Expr.AndThen(OnExpr, CoalgebraExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[Expr[F]] =
    `par`.lift.map(ParExpr(_))
}
