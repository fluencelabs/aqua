package aqua.parser.lexer

import aqua.helpers.data.PName
import aqua.helpers.data.SName
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.Comonad
import cats.parse.{Parser => P}
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.~>

case class Name[F[_]: Comonad](name: F[String]) extends Token[F] {
  override def as[T](v: T): F[T] = name.as(v)

  def asTypeToken: NamedTypeToken[F] = NamedTypeToken(name)

  override def mapK[K[_]: Comonad](fk: F ~> K): Name[K] = copy(fk(name))

  def rename(newName: String): Name[F] = copy(name.as(newName))

  def value: String = name.extract

  /*
    WARNING: This method is unsafe. `Name[S]` could be a path name
   */
  def simpleName: SName = SName.nameUnsafe(value)

  def pathName: PName = PName.stringUnsafe(value)

  override def toString() = value
}

object Name {

  type As[F[_]] = (Name[F], Option[Name[F]])

  val p: P[Name[Span.S]] =
    `name`.lift.map(Name(_))

  val variable: P[Name[Span.S]] =
    (name | Class).lift.map(Name(_))

  val upper: P[Name[Span.S]] =
    NAME.lift.map(Name(_))

  val nameAs: P[As[Span.S]] =
    asOpt(p)
}
