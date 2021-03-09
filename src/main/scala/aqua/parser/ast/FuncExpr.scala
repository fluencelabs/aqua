package aqua.parser.ast

import aqua.interim.names.NamesAlgebra
import aqua.interim.scope.PeerIdAlgebra
import aqua.interim.types.{LiteralType, TypesAlgebra}
import aqua.parser.lexer.{Arg, ArrowName, DataTypeToken, Literal, VarLambda}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import aqua.parser.lexer.Token._
import cats.syntax.comonad._
import cats.syntax.flatMap._

case class FuncExpr[F[_]](name: ArrowName[F], args: List[Arg[F]], ret: Option[DataTypeToken[F]]) extends Expr[F] {

  def program[Alg[_]](implicit T: TypesAlgebra[Alg], N: NamesAlgebra[Alg], P: PeerIdAlgebra[Alg]): Prog[Alg, Unit] =
//    Prog.around(
//      peerId match {
//        case l: Literal[F] =>
//          T.ensureTypeMatches(l, LiteralType.string, l.ts) >> P.onPeerId(l)
//        case v @ VarLambda(n, lambda) =>
//          for {
//            t <- N.read(n)
//            vt <- T.resolveLambda(t, lambda)
//            _ <- T.ensureTypeMatches(v.lambda.lastOption.getOrElse(n), LiteralType.string, vt)
//            _ <- P.onPeerId(v)
//          } yield ()
//      },
//      (_: Unit) => P.erasePeerId()
//    )
    ???

}

object FuncExpr extends Expr.AndIndented(OnExpr, AbilityIdExpr, CoalgebraExpr, ParExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[FuncExpr[F]] =
    ((`func` *> ` ` *> ArrowName.an[F]) ~ comma0(Arg.p)
      .between(`(`, `)`) ~ (`->` *> DataTypeToken.`datatypedef`).? <* ` : \n+`).map {
      case ((name, args), ret) => FuncExpr(name, args, ret)
    }
}
