package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.{ElseOtherwiseExpr, ForExpr}
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.parse.Parser
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.{Comonad, ~>}

case class ElseOtherwiseExpr[F[_]](kind: ElseOtherwiseExpr.Kind, point: Token[F])
    extends Expr[F](ElseOtherwiseExpr, point) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ElseOtherwiseExpr[K] =
    copy(point = point.mapK(fk))
}

object ElseOtherwiseExpr extends Expr.AndIndented {

  enum Kind {
    case Else, Otherwise

    def fold[A](ifElse: => A, ifOtherwise: => A): A = this match {
      case Else => ifElse
      case Otherwise => ifOtherwise
    }
  }

  override def validChildren: List[Expr.Lexem] = ForExpr.validChildren

  override val p: Parser[ElseOtherwiseExpr[Span.S]] =
    (`else`.as(Kind.Else) | `otherwise`.as(Kind.Otherwise)).lift
      .fproduct(span => Token.lift(span.as(())))
      .map { case (kind, point) =>
        ElseOtherwiseExpr(kind.extract, point)
      }
}
