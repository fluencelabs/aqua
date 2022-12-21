package aqua.builder

import aqua.backend.*
import aqua.ipfs.js.IpfsApi
import aqua.js.{CallJsFunction, FluencePeer, ServiceHandler}
import aqua.types.ScalarType
import aqua.definitions.*
import cats.data.NonEmptyList
import scribe.Logging

import scala.scalajs.js

object DeployHelper extends Logging {

  private val CreateResult = "create_result"

  private def createResult(funcName: String): AquaFunction = new AquaFunction {
    override def fnName: String = funcName

    override def handler: ServiceHandler = args => {
      val bid = args(0)
      val sid = args(1)
      js.Promise.resolve(js.Dynamic.literal(blueprint_id = bid, service_id = sid))
    }

    def arrow: ArrowTypeDef = ArrowTypeDef(
      LabeledProductTypeDef(
        ("bid", ScalarTypeDef.fromScalar(ScalarType.string)) :: (
          "sid",
          ScalarTypeDef.fromScalar(ScalarType.string)
        ) :: Nil
      ),
      UnlabeledProductTypeDef(
        StructTypeDef(
          "DeployResult",
          Map(
            "blueprint_id" -> ScalarTypeDef.fromScalar(ScalarType.string),
            "service_id" -> ScalarTypeDef.fromScalar(ScalarType.string)
          )
        ) :: Nil
      )
    )
  }

  def apply(serviceId: String = "deploy_helper"): Service = {
    val funcs = NonEmptyList.one(createResult(CreateResult))
    Service(serviceId, funcs)
  }
}
