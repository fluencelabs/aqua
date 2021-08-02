package aqua.backend.ts

import aqua.backend.{Backend, Generated}
import aqua.model.AquaContext
import aqua.model.transform.GenerationConfig
import cats.data.NonEmptyChain

object TypeScriptBackend extends Backend {

  val ext = ".ts"

  override def generate(context: AquaContext, genConf: GenerationConfig): Seq[Generated] = {
    val funcs = NonEmptyChain.fromSeq(context.funcs.values.toSeq).map(_.map(TypeScriptFunc(_)))
    funcs
      .map(fs =>
        Seq(
          Generated(
            ext,
            TypeScriptFile.Header + "\n\n" + fs
              .map(_.generateTypescript(genConf))
              .toChain
              .toList
              .mkString("\n\n")
          )
        )
      )
      .getOrElse(Seq.empty)
  }
}
