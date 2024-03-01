package aqua.compiler

import aqua.raw.ConstantRaw

/**
 * What should compiler care about during compilation – before generator backend takes its role
 *
 * @param constantsList List of known constants
 * @param relayVarName Name of the relay variable
 */
case class AquaCompilerConf(
  constants: List[ConstantRaw] = Nil,
  relayVarName: Option[String] = Some("-relay-")
)
