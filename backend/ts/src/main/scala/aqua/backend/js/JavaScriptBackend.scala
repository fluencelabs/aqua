package aqua.backend.js

import aqua.backend.ts.TypeScriptTypes
import aqua.backend.{Backend, EmptyTypes, Generated, Header, OutputFile, OutputFunc, OutputService}
import aqua.model.transform.res.AquaRes

case class JavaScriptBackend(isCommonJS: Boolean) extends Backend {

  val ext = ".js"
  val tsExt = ".d.ts"

  def typesFile(res: AquaRes): Generated = {
    val services = res.services
      .map(s => TypeScriptTypes.serviceType(s))
      .map(_.generate)
      .toList
      .mkString("\n\n")
    val functions =
      res.funcs.map(f => TypeScriptTypes.funcType(f)).map(_.generate).toList.mkString("\n\n")

    val body = s"""${Header.header(false, false)}
       |
       |// Services
       |$services
       |
       |// Functions
       |$functions
       |""".stripMargin

    Generated(tsExt, body)
  }

  override def generate(res: AquaRes): Seq[Generated] =
    if (res.isEmpty) Nil
    else {
      Generated(ext, OutputFile(res).generate(EmptyTypes, isCommonJS)):: typesFile(res) :: Nil
    }
}
