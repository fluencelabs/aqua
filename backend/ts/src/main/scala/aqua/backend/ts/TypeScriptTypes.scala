package aqua.backend.ts

import aqua.backend.{FuncTypes, ServiceTypes, Types}
import aqua.model.transform.res.{FuncRes, ServiceRes}

object TypeScriptTypes extends Types {
  override val any = ": any"
  override val fluencePeer = ": FluencePeer"
  override val string = ": string"
  override val requestFlow = ": RequestFlow"
  override def stringArr: String = ": string[]"
  override def excl: String = "!"

  override def funcType(f: FuncRes): FuncTypes = TSFuncTypes(f)

  override def serviceType(s: ServiceRes): ServiceTypes = TSServiceTypes(s)
}
