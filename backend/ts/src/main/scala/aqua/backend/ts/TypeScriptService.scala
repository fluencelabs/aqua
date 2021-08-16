package aqua.backend.ts

import aqua.model.ServiceModel

case class TypeScriptService(srv: ServiceModel) {

  def generate: String =
    s"""
       |//${srv.name}
       |
       |${srv.arrows.toMap.map { case (n, v) =>
      s"//${n}(${TypeScriptFunc.argsToTs(v)})"
    }.toList.mkString("\n")}
       |
       |""".stripMargin
}
