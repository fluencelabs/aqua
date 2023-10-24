package aqua.parser.lexer

import aqua.parser.lift.Span.S
import aqua.parser.lexer.Token.*

import cats.data.NonEmptyList
import cats.parse.Parser as P
import cats.syntax.functor.*
import cats.Comonad
import cats.arrow.FunctionK

enum NamedArg[F[_]] extends Token[F] {
  // for `name = value`
  case Full(name: Name[F], value: ValueToken[F])
  // for just `name` (short for `name = name`)
  case Short(variable: VarToken[F])

  lazy val argName: Name[F] = this match {
    case Full(name, _) => name
    case Short(variable) => variable.name
  }

  lazy val argValue: ValueToken[F] = this match {
    case Full(_, value) => value
    case Short(variable) => variable
  }

  override def as[T](v: T): F[T] = this match {
    case Full(name, value) => name.as(v)
    case Short(variable) => variable.as(v)
  }

  override def mapK[K[_]: Comonad](fk: FunctionK[F, K]): NamedArg[K] =
    this match {
      case Full(name, value) => Full(name.mapK(fk), value.mapK(fk))
      case Short(variable) => Short(variable.mapK(fk))
    }
}

object NamedArg {

  private val namedArgFull: P[NamedArg.Full[S]] =
    P.defer(
      (Name.p.between(` *`, `/s*`) <*
        `=`.between(` *`, `/s*`)) ~
        ValueToken.`value`.between(` *`, `/s*`)
    ).map(NamedArg.Full.apply)

  private val namedArgShort: P[NamedArg.Short[S]] =
    P.defer(
      VarToken.variable.between(` *`, `/s*`)
    ).map(NamedArg.Short.apply)

  private val namedArg: P[NamedArg[S]] =
    namedArgFull.backtrack | namedArgShort

  val namedArgs: P[NonEmptyList[NamedArg[S]]] =
    P.defer(` `.?.with1 ~ `(` ~ `/s*` *> comma(namedArg) <* `/s*` *> `)`)
}
