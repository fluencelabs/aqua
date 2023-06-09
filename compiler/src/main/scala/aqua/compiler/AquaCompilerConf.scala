package aqua.compiler

import aqua.raw.ConstantRaw

/**
 * What should compiler care about during compilation – before generator backend takes its role
 *
 * @param constants List of known constants
 */
case class AquaCompilerConf(constants: List[ConstantRaw] = Nil) {
  val constantsList: List[ConstantRaw] = constants ++ ConstantRaw.defaultConstants(None)
}