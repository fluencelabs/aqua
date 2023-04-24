package aqua.semantics.rules.definitions
import aqua.parser.lexer.{NamedTypeToken, Name, Token}
import aqua.semantics.lsp.TokenTypeInfo
import aqua.types.{ArrowType, Type}
import cats.data.{NonEmptyList, NonEmptyMap}

trait DefinitionsAlgebra[S[_], Alg[_]] {
  def defineDef(name: Name[S], `type`: Type): Alg[Boolean]

  def purgeDefs(token: NamedTypeToken[S]): Alg[Option[(NonEmptyMap[String, Type], Map[String, TokenTypeInfo[S]])]]

  def defineArrow(arrow: Name[S], `type`: ArrowType): Alg[Boolean]

  def purgeArrows(token: Token[S]): Alg[Option[NonEmptyList[(Name[S], ArrowType)]]]
}
