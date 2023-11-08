package aqua.semantics

import aqua.parser.Ast
import aqua.parser.lexer.Token

sealed trait SemanticError[S[_]]
case class RulesViolated[S[_]](token: Token[S], messages: List[String]) extends SemanticError[S]
case class InternalError[S[_]](messages: List[String]) extends SemanticError[S]
case class HeaderError[S[_]](token: Token[S], message: String) extends SemanticError[S]
case class WrongAST[S[_]](ast: Ast[S]) extends SemanticError[S]
