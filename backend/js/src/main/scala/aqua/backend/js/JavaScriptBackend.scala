package aqua.backend.js

import aqua.backend.{Backend, Generated}
import aqua.model.res.AquaRes
import cats.data.NonEmptyChain

object JavaScriptBackend extends Backend {

  val ext = ".js"

  override def generate(aqua: AquaRes): Seq[Generated] = {
    val funcs = NonEmptyChain.fromChain(
      aqua.funcs
        .map(JavaScriptFunc(_))
    )
    funcs
      .map(fs =>
        Seq(
          Generated(
            ext,
            JavaScriptFile.Header + "\n\n" + fs
              .map(_.generate)
              .toChain
              .toList
              .mkString("\n\n")
          )
        )
      )
      .getOrElse(Seq.empty)
  }
}
