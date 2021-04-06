package aqua.backend.ts

import cats.Show

case class TypescriptFile(funcs: Seq[TypescriptFunc])

object TypescriptFile {

  val Header: String =
    """import { FluenceClient, PeerIdB58 } from '@fluencelabs/fluence';
      |import { RequestFlowBuilder } from '@fluencelabs/fluence/dist/api.unstable';
      |""".stripMargin

  implicit val show: Show[TypescriptFile] =
    Show.show(tf => Header + "\n\n" + tf.funcs.map(_.generateTypescript).mkString("\n\n"))
}
