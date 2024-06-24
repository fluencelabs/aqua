/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.js

import aqua.*
import aqua.backend.*

import java.util.Base64
import scala.concurrent.Promise
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExportAll, JSImport}

/**
 * *
 * This is description of types from Fluence JS library.
 * See here for details https://github.com/fluencelabs/fluence-js
 */

/**
 * Particle context. Contains additional information about particle which triggered `call` air instruction from AVM
 */
trait ParticleContext {
  def particleId: String
  def initPeerId: String
  def timestamp: Int
  def ttl: Int
  def signature: String
}

object ResultCodes {
  val success = 0
  val unknownError = 1
  val exceptionInHandler = 2
}

/**
 * Represents the result of the `call` air instruction to be returned into AVM
 */
trait CallServiceResult extends js.Object {
  def retCode: Int
  def retCode_=(code: Int): Unit
  def result: js.Any
  def result_=(res: js.Any): Unit
}

/**
 * Represents the information passed from AVM when a `call` air instruction is executed on the local peer
 */
trait CallServiceData extends js.Object {
  def serviceId: String
  def fnName: String
  def args: js.Array[js.Any]
  def particleContext: ParticleContext
  def tetraplets: js.Any
}

/**
 * Information about Fluence Peer connection
 */
trait PeerStatus extends js.Object {
  def isInitialized: Boolean
  def isConnected: Boolean
  def peerId: String
  def relayPeerId: String
}

case class PeerConfig(
  connectTo: String,
  defaultTtlMs: js.UndefOr[Int],
  KeyPair: KeyPair,
  debug: js.UndefOr[Debug]
) {
  def createObj(): js.Object = {
    val d: js.Any = defaultTtlMs.asInstanceOf[js.Any]
    val deb: js.Any = debug.asInstanceOf[js.Any]
    js.Dynamic.literal(connectTo = connectTo, defaultTtlMs = d, KeyPair = KeyPair, debug = deb)
  }
}

trait AstStatus extends js.Object {
  def success: Boolean
  def data: js.Any
}

trait Internals extends js.Object {
  def parseAst(air: String): js.Promise[AstStatus]
}

/**
 * This class implements the Fluence protocol for javascript-based environments.
 * It provides all the necessary features to communicate with Fluence network
 */
@js.native
@JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v3.js", "FluencePeer")
class FluencePeer extends js.Object {
  def getStatus(): PeerStatus = js.native
  def stop(): js.Promise[Unit] = js.native
  def internals: Internals = js.native
}

object V3 {

  @js.native
  @JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v3.js", "registerService")
  def registerService(args: js.Array[js.Any], `def`: ServiceDefJs): Unit = js.native

  @js.native
  @JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v3.js", "callFunction")
  def callFunction(
    rawFnArgs: js.Array[js.Any],
    `def`: FunctionDefJs,
    script: String
  ): js.Promise[js.Any] = js.native
}

type FluenceJSLogLevel = "trace" | "debug" | "info" | "warn" | "error" | "silent"

object FluenceUtils {

  @js.native
  @JSImport("@fluencelabs/fluence", "setLogLevel")
  def setLogLevel(logLevel: FluenceJSLogLevel): Unit = js.native
}

/**
 * Public interface to Fluence JS SDK
 */
@js.native
@JSImport("@fluencelabs/fluence", "Fluence")
object Fluence extends js.Object {
  def start(config: js.UndefOr[js.Object]): js.Promise[js.Any] = js.native
  def stop(): js.Promise[js.Any] = js.native
  def getPeer(): FluencePeer = js.native
  def getStatus(): PeerStatus = js.native
}

@js.native
@JSImport("@fluencelabs/fluence", "KeyPair")
class KeyPair extends js.Object {
  val Libp2pPeerId: PeerId = js.native
  def toEd25519PrivateKey(): js.typedarray.Uint8Array = js.native
}

object KeyPairOp {

  def toDynamicJSON(kp: KeyPair) = {
    val encoder = Base64.getEncoder()
    js.Dynamic.literal(
      peerId = kp.Libp2pPeerId.toB58String(),
      secretKey = encoder.encodeToString(kp.toEd25519PrivateKey().toArray.map(s => s.toByte)),
      publicKey =
        encoder.encodeToString(kp.Libp2pPeerId.pubKey.marshal().toArray.map(s => s.toByte))
    )
  }
}

@js.native
@JSImport("@fluencelabs/fluence", "KeyPair")
object KeyPair extends js.Object {
  def fromEd25519SK(arr: js.typedarray.Uint8Array): js.Promise[KeyPair] = js.native
  def randomEd25519(): js.Promise[KeyPair] = js.native
}

@js.native
@JSImport("peer-id", "PeerId")
class PeerId extends js.Object {
  def toPrint(): String = js.native
  def toJSON(): js.Any = js.native
  val pubKey: PublicKey = js.native
  def toB58String(): String = js.native
}

@js.native
@JSImport("libp2p-crypto", "PublicKey")
class PublicKey extends js.Object {
  val bytes: js.typedarray.Uint8Array = js.native
  def marshal(): js.typedarray.Uint8Array = js.native
}
