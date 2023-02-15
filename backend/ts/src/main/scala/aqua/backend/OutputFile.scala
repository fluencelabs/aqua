package aqua.backend

import aqua.backend.ts.{TSFuncTypes, TSServiceTypes}
import aqua.backend.{Header, OutputService}
import aqua.res.AquaRes

case class OutputFile(res: AquaRes) {

  def generate(types: Types, isJs: Boolean, isOldFluenceJs: Boolean): (List[AirFunction], String) = {
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
      s"""/* eslint-disable */
         |// @ts-nocheck
         |${Header.header(isJs, isOldFluenceJs)}
         |
         |// Services
         |$services
         |// Functions
         |${functions.mkString("\n\n")}
         |
         |/* eslint-enable */""".stripMargin
    )
  }

}
