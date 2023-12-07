package aqua.semantics.rules.definitions

import aqua.parser.lexer.{Name, NamedTypeToken, Token}
import aqua.types.{ArrowType, Type}

import cats.data.{NonEmptyList, NonEmptyMap}

// Collect and purge arrows/values from structures, services, etc
trait DefinitionsAlgebra[S[_], Alg[_]] {
  def defineDef(name: Name[S], `type`: Type): Alg[Boolean]

  def purgeDefs(): Alg[Map[String, DefinitionsState.Def[S]]]

  def defineArrow(arrow: Name[S], `type`: ArrowType): Alg[Boolean]

  def purgeArrows(token: Token[S]): Alg[Option[NonEmptyList[(Name[S], ArrowType)]]]
}
