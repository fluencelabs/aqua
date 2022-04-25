package aqua.builder

import aqua.backend.*
import aqua.ipfs.js.IpfsApi
import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer, ServiceHandler}
import aqua.types.ScalarType
import cats.data.NonEmptyList
import scribe.Logging

import scala.scalajs.js

object IPFSUploader extends Logging {

  private def uploadFunc(funcName: String): AquaFunction = new AquaFunction {
    override def fnName: String = funcName

    private def logError(s: String) = logger.error(s)
    private def logInfo(s: String) = logger.info(s)

    override def handler: ServiceHandler = args => {
      IpfsApi
        .uploadFile(args(0), args(1), logError, logInfo)
        .`catch` { err =>
          js.Dynamic.literal(error = "File upload error: " + err)
        }

    }

    def arrow: ArrowTypeDef = ArrowTypeDef(
      LabeledProductTypeDef(
        ("path", ScalarTypeDef.fromScalar(ScalarType.string)) :: (
          "multiaddr",
          ScalarTypeDef.fromScalar(ScalarType.string)
        ) :: Nil
      ),
      UnlabeledProductTypeDef(
        StructTypeDef(
          "UploadResult",
          Map(
            "error" -> ScalarTypeDef.fromScalar(ScalarType.string),
            "cid" -> ScalarTypeDef.fromScalar(ScalarType.string),
            "size" -> ScalarTypeDef.fromScalar(ScalarType.u64)
          )
        ) :: Nil
      )
    )
  }

  def apply(serviceId: String, fnName: String): Service = {
    val funcs = NonEmptyList.one(uploadFunc(fnName))
    Service(serviceId, funcs)
  }
}
