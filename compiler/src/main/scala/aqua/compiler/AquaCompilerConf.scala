package aqua.compiler

import aqua.raw.ConstantRaw

/**
 * What should compiler care about during compilation – before generator backend takes its role
 *
 * @param constantsList List of known constants
 */
case class AquaCompilerConf(constantsList: List[ConstantRaw])
