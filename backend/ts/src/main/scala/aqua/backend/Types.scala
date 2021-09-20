package aqua.backend

import aqua.backend.ts.TypeScriptCommon.{fixupArgName, fnDefAdd, typeToTs, typeToTsAdd}
import aqua.model.transform.res.{FuncRes, ServiceRes}

trait Types {
  def typed(field: String, `type`: String): String
  def generic(field: String, `type`: String): String
  def bang(field: String): String
  def funcType(f: FuncRes): FuncTypes
  def serviceType(s: ServiceRes): ServiceTypes
}

trait FuncTypes {
  def exportTypes: String
  def retTypeTs: String
  def generate: String
}

trait ServiceTypes {
  def exportInterface: String
  def registerServiceArgs: String
  def generate: String
}

object EmptyTypes extends Types {
  override def typed(field: String, `type`: String): String = field
  override def generic(field: String, `type`: String): String = field
  override def bang(field: String): String = field
  override def funcType(f: FuncRes): FuncTypes = new FuncTypes {
    override def exportTypes: String = ""
    override def retTypeTs: String = ""
    override def generate: String = ""
  } 
  override def serviceType(s: ServiceRes): ServiceTypes = new ServiceTypes {
    override def exportInterface: String = ""
    override def registerServiceArgs: String = ""
    override def generate: String = ""
  }
}


