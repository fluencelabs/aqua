package aqua.ast.gen

import aqua.ast.algebra.types.ArrowType
import aqua.parser.lexer.{Ability, Name}
import cats.free.Free

abstract class ArrowGen(val `type`: ArrowType) {
  def gen[Alg[_], F[_]](result: Option[Name[F]]): Free[Alg, AirGen]
}

object ArrowGen {

  def func(`type`: ArrowType): ArrowGen =
    new ArrowGen(`type`) {

      override def gen[Alg[_], F[_]](result: Option[Name[F]]): Free[Alg, AirGen] =
        ???
    }

  def service(name: String, `type`: ArrowType): ArrowGen =
    new ArrowGen(`type`) {
      override def gen[Alg[_], F[_]](result: Option[Name[F]]): Free[Alg, AirGen] = ???
    }

  def arg(`type`: ArrowType): ArrowGen =
    new ArrowGen(`type`) {
      override def gen[Alg[_], F[_]](result: Option[Name[F]]): Free[Alg, AirGen] = ???
    }
}
