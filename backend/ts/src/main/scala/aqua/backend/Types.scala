package aqua.backend

import aqua.backend.ts.TypeScriptCommon.{fixupArgName, fnDefAdd, typeToTs, typeToTsAdd}
import aqua.model.transform.res.{FuncRes, ServiceRes}

trait Types {
  def any: String
  def excl: String
  def fluencePeer: String
  def string: String
  def stringArr: String
  def requestFlow: String
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
  override val any = ""
  override val fluencePeer = ""
  override val string = ""
  override val requestFlow = ""
  override def stringArr: String = ""
  override def excl: String = ""
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


