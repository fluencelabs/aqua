package aqua.parser.ast

import aqua.interim.abilities.AbilitiesAlgebra
import aqua.interim.names.NamesAlgebra
import aqua.interim.types.{LiteralType, TypesAlgebra}
import aqua.parser.lexer.{Ability, Literal, Value, VarLambda}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import aqua.parser.lexer.Token._
import cats.data.NonEmptyList
import cats.free.Free
import cats.syntax.comonad._

case class ServiceExpr[F[_]](name: Ability[F], id: Option[Value[F]]) extends Expr[F] {

  def program[Alg[_]](implicit
    A: AbilitiesAlgebra[Alg],
    N: NamesAlgebra[Alg],
    T: TypesAlgebra[Alg],
    F: Comonad[F]
  ): Prog[Alg, Unit] =
    Prog after A
      .purgeArrows[F]()
      .map(NonEmptyList.fromList)
      .flatMap {
        case Some(list) =>
          A.defineService(name, list.map(kv => kv._1.name.extract -> kv._2).toNem).flatMap { _ =>
            id.fold(Free.pure[Alg, Unit](())) {
              case l: Literal[F] if l.ts == LiteralType.string =>
                A.setServiceId(name, l)

              case v @ VarLambda(n, lambda) =>
                for {
                  t <- N.read(n)
                  // TODO: must be a string, but can error
                  // TODO: in case of error, highlight appropriate lambda
                  // vt <- T.resolve(t, lambda)
                  // if vt == LiteralType.string
                  _ <- A.setServiceId(name, v)

                } yield ()

              case _ =>
                // TODO error: service id must be a string
                Free.pure(())

            }

          }
        case None =>
          // TODO: error, service can't be empty
          Free.pure(())
      }

}

object ServiceExpr extends Expr.AndIndented(ArrowTypeExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[ServiceExpr[F]] =
    (`service` *> ` ` *> Ability.ab[F] ~ Value.`value`[F].between(`(`, `)`).backtrack.? <* ` : \n+`).map {
      case (name, id) => ServiceExpr(name, id)
    }
}
