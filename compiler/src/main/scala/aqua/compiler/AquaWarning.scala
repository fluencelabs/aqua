package aqua.compiler

import aqua.semantics

enum AquaWarning[S[_]] {
  case CompileWarning(warning: semantics.SemanticWarning[S])
}
