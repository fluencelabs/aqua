package aqua.backend

import aqua.backend.ts.{TSFuncTypes, TSServiceTypes}
import aqua.res.{FuncRes, ServiceRes}

trait Types {
  def typed(field: String, `type`: String): String
  def generic(field: String, `type`: String): String
  def bang(field: String): String
  def funcType(f: FuncRes): FuncTypes
  def serviceType(s: ServiceRes): ServiceTypes
}

trait FuncTypes {
  def retTypeTs: (Option[String], String)
  def generate: String
}

trait ServiceTypes {
  def generate: String
}

object EmptyTypes extends Types {
  override def typed(field: String, `type`: String): String = field
  override def generic(field: String, `type`: String): String = field
  override def bang(field: String): String = field

  override def funcType(f: FuncRes): FuncTypes = new FuncTypes {
    override def retTypeTs: (Option[String], String) = (None, "")
    override def generate: String = ""
  }

  override def serviceType(s: ServiceRes): ServiceTypes = new ServiceTypes {
    override def generate: String = ""
  }
}
