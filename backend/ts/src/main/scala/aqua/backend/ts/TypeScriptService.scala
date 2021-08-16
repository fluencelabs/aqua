package aqua.backend.ts

import aqua.model.transform.res.ServiceRes

case class TypeScriptService(srv: ServiceRes) {

  import TypeScriptFunc.typeToTs

  def generate: String =
    s"""
       |//${srv.name}
       |//defaultId = ${srv.defaultId.getOrElse("undefined")}
       |
       |${srv.members.map { case (n, v) =>
      s"//${n}: ${typeToTs(v)}"
    }.mkString("\n")}
       |//END ${srv.name}
       |
       |""".stripMargin
}
