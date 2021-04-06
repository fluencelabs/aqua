package aqua.backend.ts

import aqua.model.ScriptModel
import cats.Show
import cats.data.Chain

case class TypescriptFile(script: ScriptModel) {
  def funcs: Chain[TypescriptFunc] = script.resolveFunctions.map(TypescriptFunc)
}

object TypescriptFile {

  val Header: String =
    """import { FluenceClient, PeerIdB58 } from '@fluencelabs/fluence';
      |import { RequestFlowBuilder } from '@fluencelabs/fluence/dist/api.unstable';
      |""".stripMargin

  implicit val show: Show[TypescriptFile] =
    Show.show(tf => Header + "\n\n" + tf.funcs.map(_.generateTypescript).toList.mkString("\n\n"))
}
