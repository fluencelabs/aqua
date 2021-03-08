package aqua.parser.ast

import aqua.parser.lexer.{Arg, ArrowName, DataTypeToken}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import aqua.parser.lexer.Token._

case class DefFuncExpr[F[_]](name: ArrowName[F], args: List[Arg[F]], ret: Option[DataTypeToken[F]]) extends Expr[F]

object DefFuncExpr extends Expr.Companion {

  override def wrapsExprs: List[Expr.Companion] = OnExpr :: AbilityIdExpr :: CoalgebraExpr :: Nil

  override def p[F[_]: LiftParser: Comonad]: Parser[DefFuncExpr[F]] =
    ((`func` *> ` ` *> ArrowName.an[F]) ~ comma0(Arg.p)
      .between(`(`, `)`) ~ (`->` *> DataTypeToken.`datatypedef`).? <* ` : \n+`).map {
      case ((name, args), ret) => DefFuncExpr(name, args, ret)
    }
}
