package aqua

import scala.scalajs.js.JSON
import scala.scalajs.js

object KeyPairStringify {
  def stringify(keypair: KeyPair): String = {
    import java.util.Base64

    val encoder = Base64.getEncoder()
    val kp = js.Dynamic.literal(
        peerId = keypair.Libp2pPeerId.toB58String(),
        secretKey = encoder.encodeToString(keypair.toEd25519PrivateKey().toArray.map(s => s.toByte)),
        publicKey = encoder.encodeToString(keypair.Libp2pPeerId.pubKey.bytes.toArray.map(s => s.toByte)),
    )

    JSON.stringify(kp, space = 4)
  }
}
