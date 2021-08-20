package aqua.backend

import aqua.model.transform.res.AquaRes

/**
 * Compiler backend generates output based on the processed model
 */
trait Backend {

  /**
   * Generate the result based on the given [[AquaRes]]
   *
   * @param aqua Source file context, processed, transformed
   * @return Zero or more [[Generated]] objects, based on arguments
   */
  def generate(aqua: AquaRes): Seq[Generated]
}
