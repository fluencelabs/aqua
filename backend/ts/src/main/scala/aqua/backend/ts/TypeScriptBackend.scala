package aqua.backend.ts

import aqua.backend.{Backend, Compiled}
import aqua.model.AquaContext
import aqua.model.transform.BodyConfig
import cats.data.Chain

object TypeScriptBackend extends Backend {

  val ext = ".ts"

  override def generate(context: AquaContext, bc: BodyConfig): Seq[Compiled] = {
    val funcs = Chain.fromSeq(context.funcs.values.toSeq).map(TypeScriptFunc(_))
    val services = Chain.fromSeq(context.services.values.toSeq).map(TypeScriptService(_))
    val twoSlashn = System.lineSeparator() + System.lineSeparator()
    Seq(
      Compiled(
        ext,
        TypeScriptFile.Header
          + twoSlashn

          + services
            .map(_.generateTypescript(bc))
            .toList
            .mkString(twoSlashn)

          + funcs
            .map(_.generateTypescript(bc))
            .toList
            .mkString(twoSlashn)
      )
    )
  }
}
