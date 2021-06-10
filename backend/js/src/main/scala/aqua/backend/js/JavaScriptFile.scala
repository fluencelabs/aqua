package aqua.backend.js

import aqua.model.AquaContext
import aqua.model.transform.BodyConfig
import cats.data.Chain

case class JavaScriptFile(context: AquaContext) {

  def funcs: Chain[JavaScriptFunc] =
    Chain.fromSeq(context.funcs.values.toSeq).map(JavaScriptFunc(_))

  def generateJS(conf: BodyConfig = BodyConfig()): String =
    JavaScriptFile.Header + "\n\n" + funcs.map(_.generateTypescript(conf)).toList.mkString("\n\n")
}

object JavaScriptFile {

  val Header: String =
    s"""/**
       | *
       | * This file is auto-generated. Do not edit manually: changes may be erased.
       | * Generated by Aqua compiler: https://github.com/fluencelabs/aqua/. 
       | * If you find any bugs, please write an issue on GitHub: https://github.com/fluencelabs/aqua/issues
       | * Aqua version: ${Option(getClass.getPackage.getImplementationVersion)
      .filter(_.nonEmpty)
      .getOrElse("Unknown")}
       | *
       | */
       |import { FluenceClient, PeerIdB58 } from '@fluencelabs/fluence';
       |import { RequestFlowBuilder } from '@fluencelabs/fluence/dist/api.unstable';
       |import { RequestFlow } from '@fluencelabs/fluence/dist/internal/RequestFlow';
       |""".stripMargin

}
