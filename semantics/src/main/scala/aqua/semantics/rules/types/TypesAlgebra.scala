package aqua.semantics.rules.types

import aqua.parser.lexer.*
import aqua.raw.value.{PropertyRaw, ValueRaw}
import aqua.types.*

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.data.OptionT

trait TypesAlgebra[S[_], Alg[_]] {

  def resolveType(token: TypeToken[S]): Alg[Option[Type]]

  def resolveStreamType(token: TypeToken[S]): Alg[Option[StreamType]]

  def resolveNamedType(token: TypeToken[S]): Alg[Option[AbilityType | StructType]]

  def getType(name: String): Alg[Option[Type]]

  def resolveArrowDef(arrowDef: ArrowTypeToken[S]): Alg[Option[ArrowType]]

  def resolveServiceType(name: NamedTypeToken[S]): Alg[Option[ServiceType]]

  def defineAbilityType(
    name: NamedTypeToken[S],
    fields: Map[String, (Name[S], Type)]
  ): Alg[Option[AbilityType]]

  def defineServiceType(
    name: NamedTypeToken[S],
    fields: Map[String, (Name[S], Type)]
  ): Alg[Option[ServiceType]]

  def defineStructType(
    name: NamedTypeToken[S],
    fields: Map[String, (Name[S], Type)]
  ): Alg[Option[StructType]]

  def defineAlias(name: NamedTypeToken[S], target: Type): Alg[Boolean]

  def resolveIndex(rootT: Type, op: IntoIndex[S], idx: ValueRaw): Alg[Option[PropertyRaw]]

  def resolveCopy(
    token: IntoCopy[S],
    rootT: Type,
    fields: NonEmptyList[(NamedArg[S], ValueRaw)]
  ): Alg[Option[PropertyRaw]]

  def resolveField(rootT: Type, op: IntoField[S]): Alg[Option[PropertyRaw]]

  def resolveArrow(
    rootT: Type,
    op: IntoArrow[S],
    arguments: List[ValueRaw]
  ): Alg[Option[PropertyRaw]]

  def ensureValuesComparable(token: Token[S], left: Type, right: Type): Alg[Boolean]

  def ensureTypeMatches(token: Token[S], expected: Type, givenType: Type): Alg[Boolean]

  def ensureTypeIsCollectible(token: Token[S], givenType: Type): Alg[Boolean]

  def typeToIterable(token: Token[S], givenType: Type): OptionT[Alg, CollectionType]

  def ensureTypeOneOf[T <: Type](
    token: Token[S],
    expected: Set[T],
    givenType: Type
  ): Alg[Option[Type]]

  def checkArrowCallResults(
    token: Token[S],
    arrowType: ArrowType,
    results: List[Name[S]]
  ): Alg[Unit]

  def checkArgumentsNumber(token: Token[S], expected: Int, givenNum: Int): Alg[Boolean]

  def beginArrowScope(token: ArrowTypeToken[S]): Alg[ArrowType]

  // Check return types
  def checkArrowReturn(values: NonEmptyList[(ValueToken[S], ValueRaw)]): Alg[Boolean]

  // End scope; if return was expected but not checked, fail
  def endArrowScope(token: Token[S]): Alg[List[ValueRaw]]
}
