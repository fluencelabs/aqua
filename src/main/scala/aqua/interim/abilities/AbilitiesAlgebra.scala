package aqua.interim.abilities

import aqua.interim.types.ArrowType
import aqua.parser.lexer.{Ability, ArrowName, Value}
import cats.InjectK
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.free.Free

class AbilitiesAlgebra[Alg[_]](implicit A: InjectK[AbilityOp, Alg]) {

  def defineArrow[F[_]](arrow: ArrowName[F], `type`: ArrowType): Free[Alg, Unit] =
    Free.liftInject[Alg](DefineArrow[F](arrow, `type`))

  def purgeArrows[F[_]](): Free[Alg, NonEmptyList[(ArrowName[F], ArrowType)]] =
    Free.liftInject[Alg](PurgeArrows[F]())

  def defineService[F[_]](name: Ability[F], arrows: NonEmptyMap[String, ArrowType]): Free[Alg, Unit] =
    Free.liftInject[Alg](DefineService[F](name, arrows))

  def getArrow[F[_]](name: Ability[F], arrow: ArrowName[F]): Free[Alg, ArrowType] =
    Free.liftInject[Alg](GetArrow[F](name, arrow))

  def setServiceId[F[_]](name: Ability[F], id: Value[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](SetServiceId[F](name, id))

  def unsetServiceId[F[_]](name: Ability[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](UnsetServiceId[F](name))

}
