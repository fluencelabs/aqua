package aqua.compiler

import aqua.parser
import aqua.parser.lexer.Token
import aqua.semantics

import cats.data.NonEmptyChain

enum AquaError[+I, +E, S[_]] {
  case SourcesError(err: E)
  case ParserError(err: parser.ParserError[S])

  case ResolveImportsError(fromFile: I, token: Token[S], err: E)
  case ImportError(token: Token[S])
  case CycleError(modules: NonEmptyChain[I])

  case CompileError(err: semantics.SemanticError[S])
  case OutputError(compiled: AquaCompiled[I], err: E)
  case AirValidationError(errors: NonEmptyChain[String])
}
