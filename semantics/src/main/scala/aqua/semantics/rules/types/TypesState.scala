package aqua.semantics.rules.types

import aqua.parser.lexer.*
import aqua.raw.RawContext
import aqua.raw.value.ValueRaw
import aqua.types.*

import cats.kernel.Monoid

case class TypesState[S[_]](
  fields: Map[String, (Name[S], Type)] = Map(),
  strict: Map[String, Type] = Map.empty,
  stack: List[TypesState.Frame[S]] = Nil
) {
  def isDefined(t: String): Boolean = strict.contains(t)

  def defineType(name: NamedTypeToken[S], `type`: Type): TypesState[S] =
    copy(
      strict = strict.updated(name.value, `type`)
    )

  def getType(name: String): Option[Type] =
    strict.get(name)
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

  def init[S[_]](context: RawContext): TypesState[S] =
    TypesState(strict = context.allTypes.map { case (name, t) =>
      name.value -> t
    })
}
