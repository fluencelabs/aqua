package aqua.backend.ts

import aqua.model.ScriptModel
import aqua.model.transform.BodyConfig
import cats.data.Chain

case class TypescriptFile(script: ScriptModel) {
  def funcs: Chain[TypescriptFunc] = script.resolveFunctions.map(TypescriptFunc(_))

  def generateTS(conf: BodyConfig = BodyConfig()): String =
    TypescriptFile.Header + "\n\n" + funcs.map(_.generateTypescript(conf)).toList.mkString("\n\n")
}

object TypescriptFile {

  val Header: String =
    """/**
      | *
      | * This is generated and patched by https://github.com/fluencelabs/aqua/ code. All changes can be erased at compile time.
      | * If you found bugs - feel free to write an issue here https://github.com/fluencelabs/aqua/issues
      | *
      | */
      |import { FluenceClient, PeerIdB58 } from '@fluencelabs/fluence';
      |import { RequestFlowBuilder } from '@fluencelabs/fluence/dist/api.unstable';
      |""".stripMargin

}
