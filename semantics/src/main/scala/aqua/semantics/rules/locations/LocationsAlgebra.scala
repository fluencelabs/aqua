package aqua.semantics.rules.locations
import aqua.parser.lexer.Token
import aqua.types.Type

case class TokenInfo[S[_]](token: Token[S], `type`: Type)

trait LocationsAlgebra[S[_], Alg[_]] {
  def addToken(name: String, tokenInfo: TokenInfo[S]): Alg[Unit]
  def addTokenWithFields(name: String, token: TokenInfo[S], fields: List[(String, TokenInfo[S])]): Alg[Unit]

  def pointTokenWithFieldLocation(typeName: String, typeToken: Token[S], fieldName: String, token: Token[S]): Alg[Unit]
  def pointFieldLocation(typeName: String, fieldName: String, token: Token[S]): Alg[Unit]
  def pointLocation(name: String, token: Token[S]): Alg[Unit]
  def pointLocations(locations: List[(String, Token[S])]): Alg[Unit]

  def beginScope(): Alg[Unit]

  def endScope(): Alg[Unit]
}
