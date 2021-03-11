package aqua.ast.algebra.abilities

import aqua.ast.algebra.types.ArrowType
import aqua.parser.lexer.{Ability, Name, Token, Value}
import cats.InjectK
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.free.Free

class AbilitiesAlgebra[F[_], Alg[_]](implicit A: InjectK[AbilityOp.Aux[F, *], Alg]) {

  def defineArrow(arrow: Name[F], `type`: ArrowType): Free[Alg, Unit] =
    Free.liftInject[Alg](DefineArrow[F](arrow, `type`): AbilityOp.Aux[F, Unit])

  def purgeArrows(): Free[Alg, NonEmptyList[(Name[F], ArrowType)]] =
    Free.liftInject[Alg](PurgeArrows[F](): AbilityOp.Aux[F, NonEmptyList[(Name[F], ArrowType)]])

  def defineService(name: Ability[F], arrows: NonEmptyMap[String, ArrowType]): Free[Alg, Unit] =
    Free.liftInject[Alg](DefineService[F](name, arrows): AbilityOp.Aux[F, Unit])

  def getArrow(name: Ability[F], arrow: Name[F]): Free[Alg, ArrowType] =
    Free.liftInject[Alg](GetArrow[F](name, arrow): AbilityOp.Aux[F, ArrowType])

  def setServiceId(name: Ability[F], id: Value[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](SetServiceId[F](name, id): AbilityOp.Aux[F, Unit])

  def beginScope(token: Token[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](BeginScope[F](token): AbilityOp.Aux[F, Unit])

  def endScope(): Free[Alg, Unit] =
    Free.liftInject[Alg](EndScope[F](): AbilityOp.Aux[F, Unit])

}

object AbilitiesAlgebra {

  implicit def abilitiesAlgebra[F[_], Alg[_]](implicit A: InjectK[AbilityOp.Aux[F, *], Alg]): AbilitiesAlgebra[F, Alg] =
    new AbilitiesAlgebra[F, Alg]()
}
