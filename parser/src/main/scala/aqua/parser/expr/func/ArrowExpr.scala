package aqua.parser.expr.func

import aqua.parser.{ArrowReturnError, Ast, Expr, ParserError}
import aqua.parser.lexer.{ArrowTypeToken, DataTypeToken, TypeToken, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import cats.~>

case class ArrowExpr[F[_]](arrowTypeExpr: ArrowTypeToken[F])
    extends Expr[F](ArrowExpr, arrowTypeExpr) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ArrowExpr[K] =
    copy(arrowTypeExpr.mapK(fk))

}

object ArrowExpr extends Expr.AndIndented {

  val funcChildren: List[Expr.Lexem] =
    AbilityIdExpr ::
      PushToStreamExpr ::
      ForExpr ::
      Expr.defer(OnExpr) ::
      CallArrowExpr ::
      IfExpr ::
      TryExpr ::
      ElseOtherwiseExpr ::
      CatchExpr ::
      ParExpr ::
      CoExpr ::
      DeclareStreamExpr ::
      ClosureExpr ::
      AssignmentExpr ::
      Nil

  override val validChildren: List[Expr.Lexem] =
    ReturnExpr :: funcChildren

  override def p[F[_]: LiftParser: Comonad]: Parser[ArrowExpr[F]] =
    ArrowTypeToken
      .`arrowWithNames`[F](
        TypeToken.`typedef`[F]
      )
      .map(ArrowExpr(_))
}
