package aqua.semantics.rules.types

import aqua.parser.lexer.*
import aqua.raw.value.{PropertyRaw, ValueRaw}
import aqua.types.{ArrowType, StructType, Type}
import cats.data.NonEmptyMap
import cats.data.NonEmptyList

trait TypesAlgebra[S[_], Alg[_]] {

  def resolveType(token: TypeToken[S]): Alg[Option[Type]]

  def resolveArrowDef(arrowDef: ArrowTypeToken[S]): Alg[Option[ArrowType]]

  def defineDef(name: Name[S], `type`: Type): Alg[Boolean]

  def purgeDefs(token: CustomTypeToken[S]): Alg[Option[NonEmptyMap[String, Type]]]

  def defineType(name: CustomTypeToken[S], target: Type): Alg[Boolean]

  def resolveIndex(rootT: Type, op: IntoIndex[S], idx: ValueRaw): Alg[Option[PropertyRaw]]
  def resolveCopy(rootT: Type, op: IntoCopy[S], fields: NonEmptyMap[String, ValueRaw]): Alg[Option[PropertyRaw]]
  def resolveArrow(rootT: Type, op: IntoArrow[S], arguments: List[ValueRaw]): Alg[Option[PropertyRaw]]
  def resolveField(rootT: Type, op: IntoField[S]): Alg[Option[PropertyRaw]]

  def ensureValuesComparable(token: Token[S], left: Type, right: Type): Alg[Boolean]

  def ensureTypeMatches(token: Token[S], expected: Type, givenType: Type): Alg[Boolean]

  def expectNoExport(token: Token[S]): Alg[Unit]

  def checkArgumentsNumber(token: Token[S], expected: Int, givenNum: Int): Alg[Boolean]

  def beginArrowScope(token: ArrowTypeToken[S]): Alg[ArrowType]

  // Check return types
  def checkArrowReturn(values: NonEmptyList[(ValueToken[S], ValueRaw)]): Alg[Boolean]

  // End scope; if return was expected but not checked, fail
  def endArrowScope(token: Token[S]): Alg[List[ValueRaw]]
}
