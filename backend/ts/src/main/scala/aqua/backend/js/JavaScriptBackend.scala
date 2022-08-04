package aqua.backend.js

import aqua.backend.ts.TypeScriptTypes
import aqua.backend.*
import aqua.res.AquaRes
import cats.data.ValidatedNec
import cats.data.Validated.validNec

case class JavaScriptBackend(isCommonJS: Boolean) extends Backend {

  val ext = ".js"
  val tsExt = ".d.ts"

  def typesFile(res: AquaRes): Generated = {
    val services = res.services
      .map(s => TypeScriptTypes.serviceType(s))
      .map(_.generate)
      .toList
      .mkString("\n")
    val functions =
      res.funcs.map(f => TypeScriptTypes.funcType(f)).map(_.generate).toList.mkString("\n")

    val body = s"""${Header.header(true, false)}
                  |
                  |// Services
                  |$services
                  |
                  |// Functions
                  |$functions
                  |""".stripMargin

    Generated(tsExt, body)
  }

  override def generate(res: AquaRes, airChecker: String => ValidatedNec[String, Unit]): ValidatedNec[String, Seq[Generated]] =
    if (res.isEmpty) validNec(Nil)
    else
      OutputFile(res, airChecker)
        .generate(EmptyTypes, true, isCommonJS)
        .map(r => Generated(ext, r) :: typesFile(res) :: Nil)
}
