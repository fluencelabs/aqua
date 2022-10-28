package aqua.backend.js

import aqua.backend.ts.TypeScriptTypes
import aqua.backend.*
import aqua.res.AquaRes

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

    val body = s"""/* eslint-disable */
                  |// @ts-nocheck
                  |${Header.header(true, false)}
                  |
                  |// Services
                  |$services
                  |
                  |// Functions
                  |$functions
                  |
                  |/* eslint-enable */""".stripMargin

    Generated(tsExt, body, Nil)
  }

  override def generate(res: AquaRes): Seq[Generated] =
    if (res.isEmpty) Nil
    else {
      val (airs, script) = OutputFile(res).generate(EmptyTypes, true, isCommonJS)
      Generated(ext, script, airs) :: typesFile(res) :: Nil
    }
}
