package aqua.backend

import aqua.model.AquaContext
import aqua.model.transform.BodyConfig

/**
 * Compilation result
 * @param suffix extension or another info that will be added to a resulted file
 * @param content a code that is used as an output
 */
case class Compiled(suffix: String, content: String)

/**
 * Describes how context can be finalized.
 * If there is no functions in context - return empty seq
 */
trait Backend {
  def generate(context: AquaContext, bc: BodyConfig): Seq[Compiled]
}
