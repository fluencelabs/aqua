package aqua.backend.ts

import aqua.backend.{Backend, Compiled}
import aqua.model.AquaContext
import aqua.model.transform.BodyConfig
import cats.data.NonEmptyChain

object TypeScriptBackend extends Backend {

  val ext = ".ts"

  override def generate(context: AquaContext, bc: BodyConfig): Seq[Compiled] = {
    val funcs = NonEmptyChain.fromSeq(context.funcs.values.toSeq).map(_.map(TypeScriptFunc(_)))
    funcs
      .map(fs =>
        Seq(
          Compiled(
            ext,
            TypeScriptFile.Header + "\n\n" + fs
              .map(_.generateTypescript(bc))
              .toChain
              .toList
              .mkString("\n\n")
          )
        )
      )
      .getOrElse(Seq.empty)
  }
}
