package aqua.ast.expr

import aqua.ast.{Expr, Gen, Prog}
import aqua.ast.algebra.types.TypesAlgebra
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{CustomTypeToken, TypeToken}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.free.Free
import cats.parse.Parser
import cats.syntax.functor._

case class AliasExpr[F[_]](name: CustomTypeToken[F], target: TypeToken[F]) extends Expr[F] {

  def program[Alg[_]](implicit T: TypesAlgebra[F, Alg]): Prog[Alg, Gen] =
    T.resolveType(target).flatMap {
      case Some(t) => T.defineAlias(name, t) as Gen.noop
      case None => Gen.error.lift
    }

}

object AliasExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[Expr[F]] =
    ((`alias` *> ` ` *> CustomTypeToken.ct[F] <* ` : `) ~ TypeToken.`typedef`[F]).map {
      case (name, target) => AliasExpr(name, target)
    }
}
