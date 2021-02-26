package aqua.model

import aqua.parser.DataType
import aqua.parser.lexer.Value

trait Instr

// Fully resolved Scope must have no expected abilities (all resolved)
case class Scope(
  // None means "inherit"
  peerId: Option[Value],
  // Take vars, set vars
  importData: Map[String, DataType],
  exportData: Map[String, DataType],
  // Abilities can be imported or set
  expectedAbilities: Set[String],
  resolvedAbilities: Set[String],
  // We don't know the types yet
  expectArrows: Set[String],
  // resolved subtrees, or unresolved shit
  body: List[Either[Scope, Instr]]
)
