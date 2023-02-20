package aqua.backend.ts

import aqua.backend.{FuncTypes, ServiceTypes, Types}
import aqua.res.{FuncRes, ServiceRes}

case class TypeScriptTypes(client: String) extends Types {
  override def typed(field: String, t: String): String = s"$field: $t"
  override def generic(field: String, t: String): String = s"$field<$t>"
  override def bang(field: String): String = s"$field!"
  def funcType(f: FuncRes): FuncTypes = TSFuncTypes(f, client)
  def serviceType(s: ServiceRes): ServiceTypes = TSServiceTypes(s, client)
}
