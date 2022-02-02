package aqua.builder

import aqua.backend.{
  ArgDefinition,
  PrimitiveType,
  ServiceDef,
  ServiceFunctionDef,
  TypeDefinition,
  VoidType
}
import aqua.ipfs.js.IpfsApi
import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer, ServiceHandler}
import cats.data.NonEmptyList
import scribe.Logging

import scalajs.js

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

    override def argDefinitions: List[ArgDefinition] =
      ArgDefinition("path", PrimitiveType) :: ArgDefinition("multiaddr", PrimitiveType) :: Nil
    override def returnType: TypeDefinition = PrimitiveType
  }

  def apply(serviceId: String, fnName: String): Service = {
    val funcs = NonEmptyList.one(uploadFunc(fnName))
    Service(serviceId, funcs)
  }
}
