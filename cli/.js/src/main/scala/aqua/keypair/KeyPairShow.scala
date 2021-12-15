package aqua.keypair

import aqua.js.KeyPair
import cats.Show

import java.util.Base64
import scala.scalajs.js
import scala.scalajs.js.JSON

object KeyPairShow {
  def stringify(keypair: KeyPair): String = {
    val encoder = Base64.getEncoder()
    val kp = js.Dynamic.literal(
      peerId = keypair.Libp2pPeerId.toB58String(),
      secretKey = encoder.encodeToString(keypair.toEd25519PrivateKey().toArray.map(s => s.toByte)),
      publicKey = encoder.encodeToString(keypair.Libp2pPeerId.pubKey.bytes.toArray.map(s => s.toByte)),
    )

    JSON.stringify(kp, space = 4)
  }

  implicit val show: Show[KeyPair] = Show.show(KeyPairShow.stringify)
}
