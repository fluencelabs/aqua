package aqua.semantics

import aqua.parser.Ast
import aqua.parser.lexer.Token

sealed trait SemanticError[F[_]]
case class RulesViolated[F[_]](token: Token[F], message: String) extends SemanticError[F]
case class WrongAST[F[_]](ast: Ast[F]) extends SemanticError[F]
