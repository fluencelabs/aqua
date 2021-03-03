package aqua.context.marker

import aqua.parser.DefType
import aqua.parser.lexer.{CustomType, Token, Type}

sealed trait TypeMarker[F[_]] extends Marker[F]

case class TypeAlias[F[_], L](alias: CustomType[F], forType: Type[F]) extends TypeMarker[F] {
  override def pointer: Token[F] = alias
}

case class TypeDef[F[_], L](forDef: DefType[F, L]) extends TypeMarker[F] {
  override def pointer: Token[F] = forDef.name
}
