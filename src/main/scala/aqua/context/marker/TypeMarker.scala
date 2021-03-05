package aqua.context.marker

import aqua.parser.DefType
import aqua.parser.lexer.{CustomTypeToken, Token, TypeToken}

sealed trait TypeMarker[F[_]] extends Marker[F]

case class TypeAlias[F[_], L](alias: CustomTypeToken[F], forType: TypeToken[F]) extends TypeMarker[F] {
  override def pointer: Token[F] = alias
}

case class TypeDef[F[_], L](forDef: DefType[F, L]) extends TypeMarker[F] {
  override def pointer: Token[F] = forDef.name
}
