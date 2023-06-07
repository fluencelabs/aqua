package aqua.backend.ts

import aqua.backend.{Backend, Generated, OutputFile}
import aqua.res.AquaRes
import cats.data.NonEmptyChain

case class TypeScriptBackend(isOldFluenceJs: Boolean = false, client: String = Backend.client) extends Backend {

  override def generate(res: AquaRes): Seq[Generated] =
    if (res.isEmpty) Nil
    else {
      val (airs, script) = OutputFile(res).generate(TypeScriptTypes(client), isJs = false, isOldFluenceJs)
      Generated(TypeScriptBackend.ext, script, airs) :: Nil
    }
}

object TypeScriptBackend {
  val ext = ".ts"
}
