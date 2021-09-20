package aqua.backend.ts

import aqua.backend.{FuncTypes, ServiceTypes, Types}
import aqua.model.transform.res.{FuncRes, ServiceRes}

object TypeScriptTypes extends Types {
  override def typed(field: String, t: String): String = s"$field: $t"
  override def generic(field: String, t: String): String = s"$field<$t>"
  override def bang(field: String): String = s"$field!"

  override def funcType(f: FuncRes): FuncTypes = TSFuncTypes(f)

  override def serviceType(s: ServiceRes): ServiceTypes = TSServiceTypes(s)
}
