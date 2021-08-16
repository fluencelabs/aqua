package aqua.backend.ts

import aqua.backend.{Backend, Generated}
import aqua.model.transform.res.AquaRes
import cats.data.NonEmptyChain

object TypeScriptBackend extends Backend {

  val ext = ".ts"

  override def generate(res: AquaRes): Seq[Generated] =
    if (res.isEmpty) Nil else Generated(ext, TypeScriptFile(res).generate) :: Nil
}
