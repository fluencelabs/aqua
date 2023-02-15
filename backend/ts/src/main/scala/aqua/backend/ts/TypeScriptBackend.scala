package aqua.backend.ts

import aqua.backend.{Backend, Generated, OutputFile}
import aqua.res.AquaRes
import cats.data.NonEmptyChain

case class TypeScriptBackend(isOldFluenceJs: Boolean) extends Backend {

  val ext = ".ts"

  override def generate(res: AquaRes): Seq[Generated] =
    if (res.isEmpty) Nil
    else {
      val (airs, script) = OutputFile(res).generate(TypeScriptTypes, false, isOldFluenceJs)
      Generated(ext, script, airs) :: Nil
    }
}
