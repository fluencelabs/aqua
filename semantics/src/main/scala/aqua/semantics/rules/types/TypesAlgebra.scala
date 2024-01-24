package aqua.semantics.rules.types

import aqua.parser.lexer.*
import aqua.raw.value.{PropertyRaw, ValueRaw}
import aqua.types.*
import aqua.types.Type.*

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

  /**
   * Resolve `IntoIndex` property on value with `rootT` type
   *
   * @param op property to resolve
   * @param rootT type of the value to which property is applied
   * @param idxType type of the index
   * @return type of the value at given index if property application is valid
   */
  def resolveIntoIndex(
    op: IntoIndex[S],
    rootT: Type,
    idxType: Type
  ): Alg[Option[DataType]]

  /**
   * Resolve `IntoCopy` property on value with `rootT` type
   *
   * @param op property to resolve
   * @param rootT type of the value to which property is applied
   * @param types types of arguments passed
   * @return struct type if property application is valid
   * @note `types` should correspond to `op.args`
   */
  def resolveIntoCopy(
    op: IntoCopy[S],
    rootT: Type,
    types: NonEmptyList[Type]
  ): Alg[Option[StructType]]

  enum IntoFieldRes(`type`: Type) {
    case Field(`type`: Type) extends IntoFieldRes(`type`)
    case Property(`type`: Type) extends IntoFieldRes(`type`)

    def fold[A](field: Type => A, property: Type => A): A =
      this match {
        case Field(t) => field(t)
        case Property(t) => property(t)
      }
  }

  /**
   * Resolve `IntoField` property on value with `rootT` type
   *
   * @param op property to resolve
   * @param rootT type of the value to which property is applied
   * @return if property application is valid, return
   *         Field(type) if it's a field of rootT (fields of structs or abilities),
   *         Property(type) if it's a property of rootT (functors of collections)
   */
  def resolveIntoField(
    op: IntoField[S],
    rootT: Type
  ): Alg[Option[IntoFieldRes]]

  /**
   * Resolve `IntoArrow` property on value with `rootT` type
   *
   * @param op property to resolve
   * @param rootT type of the value to which property is applied
   * @param types types of arguments passed
   * @return arrow type if property application is valid
   * @note `types` should correspond to `op.arguments`
   */
  def resolveIntoArrow(
    op: IntoArrow[S],
    rootT: Type,
    types: List[Type]
  ): Alg[Option[ArrowType]]

  def ensureValuesComparable(token: Token[S], left: Type, right: Type): Alg[Boolean]

  def ensureTypeMatches(token: Token[S], expected: Type, givenType: Type): Alg[Boolean]

  /**
   * Check if given type (ability or struct)
   * can be constructed from given arguments
   *
   * @param token token of construction expression (for error reporting)
   * @param expected type to construct
   * @param arguments arguments to construct with (name -> (named arg, type))
   * @return true if type can be constructed from given arguments
   * reports error and warnings if necessary
   */
  def ensureTypeConstructibleFrom(
    token: Token[S],
    expected: AbilityType | StructType,
    arguments: NonEmptyMap[String, (NamedArg[S], Type)]
  ): Alg[Boolean]

  def typeToCollectible(token: Token[S], givenType: Type): OptionT[Alg, CollectibleType]

  def typeToStream(token: Token[S], givenType: Type): OptionT[Alg, StreamType]

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
