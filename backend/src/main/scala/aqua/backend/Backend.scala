package aqua.backend

import aqua.model.AquaContext
import aqua.model.transform.GenerationConfig

/**
 * Compiler backend generates output based on the processed model
 */
trait Backend {

  /**
   * Generate the result based on the given [[AquaContext]] and [[GenerationConfig]]
   *
   * @param context Source file context, processed, transformed
   * @param genConf Generation configuration
   * @return Zero or more [[Generated]] objects, based on arguments
   */
  def generate(context: AquaContext, genConf: GenerationConfig): Seq[Generated]
}
