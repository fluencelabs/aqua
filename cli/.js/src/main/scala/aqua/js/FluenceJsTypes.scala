package aqua.js

import aqua.*
import aqua.backend.*

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

trait Internals extends js.Object {
  def initiateFlow(r: RequestFlow): js.Promise[js.Any]
  def callServiceHandler: CallServiceHandler
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

@JSExportAll
case class LabeledProductTypeDefJs(fields: Map[String, TypeDefinition])

@JSExportAll
case class ArrowTypeDefJs(
  domain: ProductTypeDef,
  codomain: ProductTypeDef,
  private val tag: String = "arrow"
)

object ArrowTypeDefJs {
  def apply(at: ArrowTypeDef): ArrowTypeDefJs = ArrowTypeDefJs(at.domain, at.codomain)
}

@JSExportAll
case class FunctionDefJs(
  functionName: String,
  arrow: ArrowTypeDefJs,
  names: NamesConfigJs
)

object FunctionDefJs {

  def apply(fd: FunctionDef): FunctionDefJs = {
    FunctionDefJs(
      fd.functionName,
      ArrowTypeDefJs(fd.arrow),
      NamesConfigJs(fd.names)
    )
  }
}

@JSExportAll
case class TypeDefinitionJs(tag: String)

object TypeDefinitionJs {
  def apply(td: TypeDefinition): TypeDefinitionJs = TypeDefinitionJs(td.tag)
}

@JSExportAll
case class ServiceDefJs(
  defaultServiceId: Option[String],
  functions: LabeledProductTypeDefJs
)

object ServiceDefJs {

  def apply(sd: ServiceDef): ServiceDefJs = {
    ServiceDefJs(sd.defaultServiceId, LabeledProductTypeDefJs(sd.functions.fields.toMap))
  }
}

@JSExportAll
case class NamesConfigJs(
  relay: String,
  getDataSrv: String,
  callbackSrv: String,
  responseSrv: String,
  responseFnName: String,
  errorHandlingSrv: String,
  errorFnName: String
)

object NamesConfigJs {

  def apply(nc: NamesConfig): NamesConfigJs = {
    NamesConfigJs(
      nc.relay,
      nc.getDataSrv,
      nc.callbackSrv,
      nc.responseSrv,
      nc.responseFnName,
      nc.errorHandlingSrv,
      nc.errorFnName
    )
  }
}

type AvmLogLevel = "trace" | "debug" | "info" | "warn" | "error" | "off"

@JSExportAll
case class PeerConfig(
  connectTo: String,
  defaultTtlMs: js.UndefOr[Int],
  avmLogLevel: AvmLogLevel,
  KeyPair: KeyPair
)

/**
 * This class implements the Fluence protocol for javascript-based environments.
 * It provides all the necessary features to communicate with Fluence network
 */
@js.native
@JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v1.js", "FluencePeer")
class FluencePeer extends js.Object {
  val internals: Internals = js.native
  def getStatus(): PeerStatus = js.native
  def stop(): js.Promise[Unit] = js.native
}

object V2 {

  @js.native
  @JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v2.js", "registerService")
  def registerService(args: js.Array[js.Any], `def`: ServiceDefJs): Unit = js.native

  @js.native
  @JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v2.js", "callFunction")
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
  def start(config: PeerConfig): js.Promise[js.Any] = js.native
  def stop(): js.Promise[js.Any] = js.native
  def getPeer(): FluencePeer = js.native
  def getStatus(): PeerStatus = js.native
}

/**
 * Class defines the handling of a `call` air intruction executed by AVM on the local peer.
 * All the execution process is defined by the chain of middlewares - architecture popular among backend web frameworks.
 */
@js.native
@JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v1.js", "CallServiceHandler")
class CallServiceHandler extends js.Object {

  def on(
    serviceId: String,
    fnName: String,
    handler: js.Function2[js.Array[js.Any], js.Any, js.Any]
  ): js.Function0[CallServiceHandler] = js.native

  def onEvent(
    serviceId: String,
    fnName: String,
    handler: js.Function2[js.Array[js.Any], js.Any, js.Any]
  ): js.Function0[CallServiceHandler] = js.native

  def use(
    f: js.Function3[CallServiceData, CallServiceResult, js.Function0[Unit], Unit]
  ): CallServiceHandler = js.native
}

/**
 * The class represents the current view (and state) of distributed the particle execution process from client's point of view.
 * It stores the intermediate particles state during the process. RequestFlow is identified by the id of the particle that is executed during the flow.
 * Each RequestFlow contains a separate (unique to the current flow) CallServiceHandler where the handling of `call` AIR instruction takes place
 * Please note, that RequestFlow's is handler is combined with the handler from client before the execution occures.
 * After the combination middlewares from RequestFlow are executed before client handler's middlewares.
 */
@js.native
@JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v1.js", "RequestFlow")
class RequestFlow extends js.Object {}

/**
 * Builder class for configuring and creating Request Flows
 */
@js.native
@JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v1.js", "RequestFlowBuilder")
class RequestFlowBuilder extends js.Object {
  def withRawScript(air: String): RequestFlowBuilder = js.native

  def configHandler(f: js.Function2[CallServiceHandler, js.Any, Unit]): RequestFlowBuilder =
    js.native
  def disableInjections(): RequestFlowBuilder = js.native
  def build(): RequestFlow = js.native
  def handleScriptError(f: js.Function1[js.Any, Unit]): RequestFlowBuilder = js.native
  def handleTimeout(f: js.Function0[Unit]): RequestFlowBuilder = js.native
}

@js.native
@JSImport("@fluencelabs/fluence", "KeyPair")
class KeyPair extends js.Object {
  val Libp2pPeerId: PeerId = js.native
  def toEd25519PrivateKey(): js.typedarray.Uint8Array = js.native
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
  val pubKey: PublicKey = js.native
  def toB58String(): String = js.native
}

@js.native
@JSImport("libp2p-crypto", "PublicKey")
class PublicKey extends js.Object {
  val bytes: js.typedarray.Uint8Array = js.native
}
