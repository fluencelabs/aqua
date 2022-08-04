package aqua.backend

import aqua.backend.ts.{TSFuncTypes, TSServiceTypes}
import aqua.backend.{Header, OutputService}
import aqua.res.AquaRes
import cats.syntax.traverse.*
import cats.data.ValidatedNec

case class OutputFile(res: AquaRes, airChecker: String => ValidatedNec[String, Unit]) {

  def generate(types: Types, isJs: Boolean, isCommonJS: Boolean): ValidatedNec[String, String] = {
    import types.*
    val services = res.services
      .map(s => OutputService(s, types))
      .map(_.generate)
      .toList
      .mkString("\n\n")

    res.funcs
      .map(f => OutputFunc(f, types, airChecker))
      .map(_.generate)
      .sequence
      .map(_.toList.mkString("\n\n"))
      .map { functions =>
        s"""${Header.header(isJs, isCommonJS)}
           |
           |// Services
           |$services
           |// Functions
           |$functions
           |""".stripMargin
      }

  }

}
