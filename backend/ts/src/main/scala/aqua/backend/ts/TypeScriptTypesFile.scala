package aqua.backend.ts

import aqua.backend.Header
import aqua.res.AquaRes

case class TypeScriptTypesFile(res: AquaRes, isOldFluenceJs: Boolean) {
  def generate: String =
    s"""/* eslint-disable */
       |// @ts-nocheck
       |${Header.header(false, isOldFluenceJs)}
       |
       |// Services
       |${res.services.map(TSServiceTypes(_)).map(_.generate).toList.mkString("\n\n")}
       |
       |// Functions
       |${res.funcs.map(TSFuncTypes(_)).map(_.generate).toList.mkString("\n\n")}
       |
       |/* eslint-enable */""".stripMargin
}
