package aqua

import scala.concurrent.Promise
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

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

trait CallServiceResult extends js.Object {
  def retCode: Int
  def retCode_=(code: Int): Unit
  def result: js.Any
  def result_=(res: js.Any): Unit
}

trait CallServiceData extends js.Object {
  def serviceId: String
  def fnName: String
  def args: js.Array[js.Any]
  def particleContext: ParticleContext
  def tetraplets: js.Any
}

trait Internals extends js.Object {
  def initiateFlow(r: RequestFlow): Promise[js.Any]
  def callServiceHandler: CallServiceHandler
}

trait Status extends js.Object {
  def relayPeerId: String
}

@js.native
@JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v1.js", "FluencePeer")
class FluencePeer extends js.Object {
  val internals: Internals = js.native
  def getStatus(): Status = js.native
}

@js.native
@JSImport("@fluencelabs/fluence", "Fluence")
object Fluence extends js.Object {
  def start(str: String): js.Promise[js.Any] = js.native
  def getPeer(): FluencePeer = js.native
}

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

  def use(f: js.Function3[CallServiceData, CallServiceResult, js.Function0[Unit], Unit]): CallServiceHandler = js.native
}

@js.native
@JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v1.js", "RequestFlow")
class RequestFlow extends js.Object {}

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
