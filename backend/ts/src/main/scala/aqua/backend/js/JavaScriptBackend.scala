package aqua.backend.js

import aqua.backend.ts.TypeScriptTypes
import aqua.backend.*
import aqua.res.AquaRes

case class JavaScriptBackend(isOldFluenceJs: Boolean = false, client: String = Backend.client) extends Backend {
  val types = TypeScriptTypes(client)

  def typesFile(res: AquaRes): Generated = {
    val services = res.services
      .map(s => types.serviceType(s))
      .map(_.generate)
      .toList
      .mkString("\n")
    val functions =
      res.funcs.map(f => types.funcType(f)).map(_.generate).toList.mkString("\n")

    val body = s"""/* eslint-disable */
                  |// @ts-nocheck
                  |${Header.header(true, isOldFluenceJs)}
                  |
                  |// Services
                  |$services
                  |
                  |// Functions
                  |$functions
                  |
                  |/* eslint-enable */""".stripMargin

    Generated(JavaScriptBackend.dtsExt, body, Nil)
  }

  override def generate(res: AquaRes): Seq[Generated] =
    if (res.isEmpty) Nil
    else {
      val (airs, script) = OutputFile(res).generate(EmptyTypes, true, isOldFluenceJs)
      Generated(JavaScriptBackend.ext, script, airs) :: typesFile(res) :: Nil
    }
}

object JavaScriptBackend {
  val ext = ".js"
  val dtsExt = ".d.ts"
}
