package aqua.backend.ts

import aqua.backend.Header
import aqua.model.transform.res.AquaRes

case class TypeScriptTypesFile(res: AquaRes) {
  def generate: String =
    s"""${Header.header(false, false)}
       |
       |// Services
       |${res.services.map(TSServiceTypes(_)).map(_.generate).toList.mkString("\n\n")}
       |
       |// Functions
       |${res.funcs.map(TSFuncTypes(_)).map(_.generate).toList.mkString("\n\n")}
       |""".stripMargin
}
