package aqua.ast.expr

import aqua.ast.{AirGen, Expr, Gen, ParGen, Prog}
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.Parser

case class ParExpr[F[_]](point: F[Unit]) extends Expr[F] {

  def program[Alg[_]]: Prog[Alg, Gen] =
    Prog.after[Alg, Gen] {
      case g: AirGen => ParGen(left = None, right = g).lift
      case g => g.lift
    }
}

object ParExpr extends Expr.AndThen(OnExpr, CoalgebraExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[Expr[F]] =
    `par`.lift.map(ParExpr(_))
}
