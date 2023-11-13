package aqua.semantics.rules.types

import aqua.parser.lexer.*
import aqua.raw.RawContext
import aqua.raw.value.{FunctorRaw, IntoIndexRaw, LiteralRaw, PropertyRaw, ValueRaw}
import aqua.types.*

import cats.data.Validated.{Invalid, Valid}
import cats.data.{Chain, NonEmptyChain, ValidatedNec}
import cats.kernel.Monoid
import cats.syntax.apply.*
import cats.syntax.bifunctor.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.syntax.validated.*

case class TypesState[S[_]](
  fields: Map[String, (Name[S], Type)] = Map(),
  strict: Map[String, Type] = Map.empty,
  definitions: Map[String, NamedTypeToken[S]] = Map(),
  stack: List[TypesState.Frame[S]] = Nil
) {
  def isDefined(t: String): Boolean = strict.contains(t)

  def defineType(name: NamedTypeToken[S], `type`: Type): TypesState[S] =
    copy(
      strict = strict.updated(name.value, `type`),
      definitions = definitions.updated(name.value, name)
    )

  def getType(name: String): Option[Type] =
    strict.get(name)

  def getTypeDefinition(name: String): Option[NamedTypeToken[S]] =
    definitions.get(name)
}

object TypesState {

  final case class TypeDefinition[S[_]](
    token: NamedTypeToken[S],
    `type`: Type
  )

  case class Frame[S[_]](
    token: ArrowTypeToken[S],
    arrowType: ArrowType,
    retVals: Option[List[ValueRaw]]
  )

  given [S[_]]: Monoid[TypesState[S]] with {
    override def empty: TypesState[S] = TypesState()

    override def combine(x: TypesState[S], y: TypesState[S]): TypesState[S] =
      TypesState(
        strict = x.strict ++ y.strict,
        definitions = x.definitions ++ y.definitions
      )
  }

  def init[S[_]](context: RawContext): TypesState[S] =
    TypesState(strict = context.allTypes)
}
