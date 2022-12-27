package aqua.keypair

import aqua.js.{KeyPair, KeyPairOp}
import cats.Show

import java.util.Base64
import scala.scalajs.js
import scala.scalajs.js.JSON

object KeyPairShow {

  def stringify(keypair: KeyPair): String = {
    JSON.stringify(KeyPairOp.toDynamicJSON(keypair), space = 4)
  }

  implicit val show: Show[KeyPair] = Show.show(KeyPairShow.stringify)
}
