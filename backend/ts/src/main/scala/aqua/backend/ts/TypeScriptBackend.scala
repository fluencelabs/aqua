package aqua.backend.ts

import aqua.backend.{Backend, Generated, OutputFile}
import aqua.res.AquaRes
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.data.Validated.validNec

object TypeScriptBackend extends Backend {

  val ext = ".ts"

  override def generate(res: AquaRes, airChecker: String => ValidatedNec[String, Unit]): ValidatedNec[String, Seq[Generated]] =
    if (res.isEmpty) validNec(Nil)
    else
      OutputFile(res, airChecker).generate(TypeScriptTypes, false, false).map { r =>
        Generated(
          ext,
          r
        ) :: Nil
      }
}
