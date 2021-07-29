package aqua.backend.js

import aqua.backend.{Backend, Compiled}
import aqua.model.AquaContext
import aqua.model.transform.BodyConfig
import cats.data.NonEmptyChain

object JavaScriptBackend extends Backend {

  val ext = ".js"

  override def generate(context: AquaContext, bc: BodyConfig): Seq[Compiled] = {
    val funcs = NonEmptyChain.fromSeq(context.funcs.values.toSeq).map(_.map(JavaScriptFunc(_)))
    funcs
      .map(fs =>
        Seq(
          Compiled(
            ext,
            JavaScriptFile.Header + "\n\n" + fs
              .map(_.generateJavascript(bc))
              .toChain
              .toList
              .mkString("\n\n")
          )
        )
      )
      .getOrElse(Seq.empty)
  }
}
