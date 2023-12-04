package aqua.semantics.rules.locations
import aqua.parser.lexer.Token
import aqua.types.Type

case class ExprInfo[S[_]](token: Token[S], `type`: Type)

trait LocationsAlgebra[S[_], Alg[_]] {
  def addToken(name: String, tokenInfo: ExprInfo[S]): Alg[Unit]
  def addTokenWithFields(name: String, token: ExprInfo[S], fields: List[(String, ExprInfo[S])]): Alg[Unit]

  def pointTokenWithFieldLocation(typeName: String, typeToken: Token[S], fieldName: String, token: Token[S]): Alg[Unit]
  def pointFieldLocation(typeName: String, fieldName: String, token: Token[S]): Alg[Unit]
  def pointLocation(name: String, token: Token[S]): Alg[Unit]
  def pointLocations(locations: List[(String, Token[S])]): Alg[Unit]
}
