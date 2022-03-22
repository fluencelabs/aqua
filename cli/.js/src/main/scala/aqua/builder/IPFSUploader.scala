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

    override def handler: ServiceHandler = args => {
      IpfsApi
        .uploadFile(args(0), args(1), logger.info: String => Unit, logger.error: String => Unit)
        .`catch` { err =>
          js.Dynamic.literal(error = "Error on uploading file: " + err)
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
