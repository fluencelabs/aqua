package aqua.semantics.lsp

import aqua.parser.lexer.Token
import aqua.semantics.{TokenInfo, TokenType}
import aqua.types.{ArrowType, Type}

// Token description with it's definition, type, etc
sealed trait TokenInfo[F[_]] {
  def definition: Option[Token[F]]
}

case class TokenDef[F[_]](definition: Option[Token[F]]) extends TokenInfo[F]

sealed trait TokenType[F[_]] extends TokenInfo[F] {
  def definition: Option[Token[F]]
  def tokenType: Type
}

case class TokenTypeInfo[F[_]](definition: Option[Token[F]], tokenType: Type) extends TokenType[F]

case class TokenArrowInfo[F[_]](definition: Option[Token[F]], tokenType: ArrowType)
    extends TokenType[F]
