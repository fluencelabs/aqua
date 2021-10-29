package aqua.semantics.rules.types

import aqua.model.LambdaModel
import aqua.parser.lexer._
import aqua.types.{ArrowType, Type}
import cats.data.NonEmptyMap

trait TypesAlgebra[S[_], Alg[_]] {

  def resolveType(token: TypeToken[S]): Alg[Option[Type]]

  def resolveArrowDef(arrowDef: ArrowTypeToken[S]): Alg[Option[ArrowType]]

  def defineField(name: Name[S], `type`: Type): Alg[Boolean]

  def purgeFields(token: Token[S]): Alg[Option[NonEmptyMap[String, Type]]]

  def defineDataType(
    name: CustomTypeToken[S],
    fields: NonEmptyMap[String, Type]
  ): Alg[Boolean]

  def defineAlias(name: CustomTypeToken[S], target: Type): Alg[Boolean]

  def resolveLambda(root: Type, ops: List[LambdaOp[S]]): Alg[List[LambdaModel]]

  def ensureTypeMatches(token: Token[S], expected: Type, givenType: Type): Alg[Boolean]

  def expectNoExport(token: Token[S]): Alg[Unit]

  def checkArgumentsNumber(token: Token[S], expected: Int, givenNum: Int): Alg[Boolean]
}
