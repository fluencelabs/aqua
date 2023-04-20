package aqua.semantics.rules.locations
import aqua.parser.lexer.Token
import aqua.semantics.lsp.{TokenInfo, TokenType}

trait LocationsAlgebra[S[_], Alg[_]] {
  def addNameLocations(locs: List[(Token[S], TokenType[S])]): Alg[Unit]
  def addNameLocation(token: Token[S], tokenType: TokenType[S]): Alg[Unit]

  def addTypeLocations(locs: List[(Token[S], TokenInfo[S])]): Alg[Unit]
  def addTypeLocation(token: Token[S], tokenInfo: TokenInfo[S]): Alg[Unit]

  def addServiceLocations(locs: List[(Token[S], TokenInfo[S])]): Alg[Unit]
  def addServiceLocation(token: Token[S], tokenInfo: TokenInfo[S]): Alg[Unit]
}
