package aqua.backend

import aqua.backend.ts.{TSFuncTypes, TSServiceTypes}
import aqua.backend.{Header, OutputService}
import aqua.model.transform.res.AquaRes

case class OutputFile(res: AquaRes) {

  def generate(types: Types, isCommonJS: Boolean): String = {
    import types.*
    val services = res.services
      .map(s => OutputService(s, types))
      .map(_.generate)
      .toList
      .mkString("\n\n")
    val functions =
      res.funcs.map(f => OutputFunc(f, types)).map(_.generate).toList.mkString("\n\n")
    s"""${Header.header(false, isCommonJS)}
       |
       |function ${typed(
      s"""missingFields(${typed("obj", "any")}, ${typed("fields", "string[]")})""", 
      "string[]")} {
       |    return fields.filter(f => !(f in obj))
       |}
       |
       |// Services
       |$services
       |// Functions
       |$functions
       |""".stripMargin
  }

}
