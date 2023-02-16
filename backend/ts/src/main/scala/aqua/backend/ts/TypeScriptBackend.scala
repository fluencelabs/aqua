package aqua.backend.ts

import aqua.backend.{Backend, Generated, OutputFile}
import aqua.res.AquaRes
import cats.data.NonEmptyChain

case class TypeScriptBackend(isOldFluenceJs: Boolean, client: String) extends Backend {

  val ext = ".ts"

  override def generate(res: AquaRes): Seq[Generated] =
    if (res.isEmpty) Nil
    else {
      val (airs, script) = OutputFile(res).generate(TypeScriptTypes(client), false, isOldFluenceJs)
      Generated(ext, script, airs) :: Nil
    }
}
