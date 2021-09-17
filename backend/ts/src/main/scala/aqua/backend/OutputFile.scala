package aqua.backend

import aqua.backend.ts.{TSFuncTypes, TSServiceTypes}
import aqua.backend.{Header, OutputService}
import aqua.model.transform.res.AquaRes

case class OutputFile(res: AquaRes) {

  def generate(types: Types): String = {
    import types.*
    val services = res.services
      .map(s => OutputService(s, types))
      .map(_.generate)
      .toList
      .mkString("\n\n")
    val functions =
      res.funcs.map(f => OutputFunc(f, types)).map(_.generate).toList.mkString("\n\n")
    s"""${Header.header(false)}
       |
       |function missingFields(obj$any, fields$stringArr)$stringArr {
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
