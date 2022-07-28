package aqua.backend

import aqua.backend.ts.{TSFuncTypes, TSServiceTypes}
import aqua.backend.{Header, OutputService}
import aqua.res.AquaRes

case class OutputFile(res: AquaRes) {

  def generate(types: Types, isJs: Boolean, isCommonJS: Boolean): String = {
    import types.*
    val services = res.services
      .map(s => OutputService(s, types))
      .map(_.generate)
      .toList
      .mkString("\n\n")
    val functions =
      res.funcs.map(f => OutputFunc(f, types)).map(_.generate).toList.mkString("\n\n")
    s"""${Header.header(isJs, isCommonJS)}
       |
       |// Services
       |$services
       |// Functions
       |$functions
       |""".stripMargin
  }

}
