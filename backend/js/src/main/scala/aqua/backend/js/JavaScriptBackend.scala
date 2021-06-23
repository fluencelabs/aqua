package aqua.backend.js

import aqua.backend.{Backend, Compiled}
import aqua.model.AquaContext
import aqua.model.transform.BodyConfig
import cats.data.Chain

object JavaScriptBackend extends Backend {

  val ext = ".js"

  override def generate(context: AquaContext, bc: BodyConfig): Seq[Compiled] = {
    val funcs = Chain.fromSeq(context.funcs.values.toSeq).map(JavaScriptFunc(_))
    Seq(
      Compiled(
        ext,
        JavaScriptFile.Header + "\n\n" + funcs.map(_.generateTypescript(bc)).toList.mkString("\n\n")
      )
    )
  }
}
