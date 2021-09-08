package aqua.backend.js

import aqua.backend.{Backend, Generated}
import aqua.model.transform.res.AquaRes
import cats.data.NonEmptyChain

object JavaScriptBackend extends Backend {

  val ext = ".js"

  override def generate(res: AquaRes): Seq[Generated] =
    if (res.isEmpty) Nil else Generated(ext, JavaScriptFile(res).generate) :: Nil
}
