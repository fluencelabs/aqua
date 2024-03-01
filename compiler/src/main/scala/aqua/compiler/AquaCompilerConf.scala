package aqua.compiler

import aqua.raw.ConstantRaw

/**
 * What should compiler care about during compilation – before generator backend takes its role
 *
 * @param constantsList List of known constants
 * @param noXor Disable error bubbling mechanism
 */
case class AquaCompilerConf(
  constants: List[ConstantRaw],
  noXor: Boolean = false,
  relayVarName: Option[String] = Some("-relay-")
)
