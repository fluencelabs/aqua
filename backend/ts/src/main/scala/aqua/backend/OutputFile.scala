package aqua.backend

import aqua.backend.ts.{TSFuncTypes, TSServiceTypes}
import aqua.backend.{Header, OutputService}
import aqua.res.AquaRes

case class OutputFile(res: AquaRes) {

  def generate(types: Types, isJs: Boolean, isCommonJS: Boolean): (List[AirFunction], String) = {
    import types.*
    val services = res.services
      .map(s => OutputService(s, types))
      .map(_.generate)
      .toList
      .mkString("\n\n")
    val scripts =
      res.funcs.map(f => OutputFunc(f, types)).map(_.generate)

    val (airs, functions) = scripts.toList.unzip

    (
      airs,
      s"""${Header.header(isJs, isCommonJS)}
         |
         |// Services
         |$services
         |// Functions
         |${functions.mkString("\n\n")}
         |""".stripMargin
    )
  }

}
