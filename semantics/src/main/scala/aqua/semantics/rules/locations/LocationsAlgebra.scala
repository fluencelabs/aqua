package aqua.semantics.rules.locations
import aqua.parser.lexer.Token
import aqua.semantics.lsp.{TokenInfo, TokenType}

trait LocationsAlgebra[S[_], Alg[_]] {
  def addNameLocation(token: Token[S], tokenType: TokenType[S]): Alg[Unit]
  def addTypeLocation(token: Token[S], tokenType: TokenInfo[S]): Alg[Unit]
  def addServiceLocation(token: Token[S], tokenType: TokenInfo[S]): Alg[Unit]
}
