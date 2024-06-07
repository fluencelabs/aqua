package aqua.js

import aqua.*
import aqua.backend.*
import aqua.definitions.*
import aqua.res.FuncRes
import aqua.types.*
import io.circe.{Encoder, Json}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExportAll, JSImport, JSName}

class FunctionDefJs(
  val functionName: String,
  val arrow: ArrowTypeDefJs,
  val names: NamesConfigJs
) extends js.Object

object FunctionDefJs {

  def apply(fd: FunctionDef): FunctionDefJs = {
    new FunctionDefJs(
      fd.functionName,
      ArrowTypeDefJs(
        TypeDefinitionJs(fd.arrow.domain),
        TypeDefinitionJs(fd.arrow.codomain),
        fd.arrow.tag
      ),
      NamesConfigJs(fd.names)
    )
  }
}

sealed trait TypeDefinitionJs extends js.Object

class ArrayTypeDefJs(val `type`: TypeDefinitionJs, val tag: String) extends TypeDefinitionJs

class OptionTypeDefJs(val `type`: TypeDefinitionJs, val tag: String) extends TypeDefinitionJs

class ScalarTypeDefJs(val name: String, val tag: String) extends TypeDefinitionJs

class StructTypeDefJs(
  val name: String,
  val fields: js.Dictionary[TypeDefinitionJs],
  val tag: String
) extends TypeDefinitionJs

class LabeledTypeDefJs(val fields: js.Dictionary[TypeDefinitionJs], val tag: String)
    extends TypeDefinitionJs

object LabeledTypeDefJs {

  def apply(l: LabeledProductTypeDef): LabeledTypeDefJs = {

    new LabeledTypeDefJs(
      js.Dictionary[TypeDefinitionJs](l.fields.map { case (n, t) => (n, TypeDefinitionJs(t)) }: _*),
      l.tag
    )
  }
}

class UnlabeledTypeDefJs(val items: js.Array[TypeDefinitionJs], val tag: String)
    extends TypeDefinitionJs

class TopTypeDefJs(val tag: String) extends TypeDefinitionJs

class BottomTypeDefJs(val tag: String) extends TypeDefinitionJs

class NilTypeDefJs(val tag: String) extends TypeDefinitionJs

class ArrowTypeDefJs(
  val domain: TypeDefinitionJs,
  val codomain: TypeDefinitionJs,
  val tag: String
) extends TypeDefinitionJs

object TypeDefinitionJs {

  def apply(td: TypeDefinition): TypeDefinitionJs = td match {
    case o @ OptionTypeDef(t) => new OptionTypeDefJs(apply(t), o.tag)
    case a @ ArrayTypeDef(t) => new ArrayTypeDefJs(apply(t), a.tag)
    case s @ ScalarTypeDef(n) => new ScalarTypeDefJs(n, s.tag)
    case s @ StructTypeDef(n, f) =>
      new StructTypeDefJs(
        n,
        js.Dictionary[TypeDefinitionJs](f.toList.map { case (n, t) =>
          (n, TypeDefinitionJs(t))
        }: _*),
        s.tag
      )
    case l: LabeledProductTypeDef =>
      LabeledTypeDefJs(l)
    case u @ UnlabeledProductTypeDef(items) =>
      new UnlabeledTypeDefJs(items.map(TypeDefinitionJs.apply).toJSArray, u.tag)
    case a @ ArrowTypeDef(domain, codomain) =>
      new ArrowTypeDefJs(apply(domain), apply(codomain), a.tag)
    case n @ NilTypeDef => new NilTypeDefJs(n.tag)
    case n @ TopTypeDef => new TopTypeDefJs(n.tag)
    case n @ BottomTypeDef => new BottomTypeDefJs(n.tag)
  }
}

class ServiceDefJs(
  val defaultServiceId: js.UndefOr[String],
  val functions: LabeledTypeDefJs
) extends js.Object

object ServiceDefJs {

  def apply(sd: ServiceDef): ServiceDefJs = {
    new ServiceDefJs(
      sd.defaultServiceId.getOrElse(()),
      LabeledTypeDefJs(sd.functions)
    )
  }
}

class NamesConfigJs(
  val relay: String,
  val getDataSrv: String,
  val callbackSrv: String,
  val responseSrv: String,
  val responseFnName: String,
  val errorHandlingSrv: String,
  val errorFnName: String
) extends js.Object

object NamesConfigJs {

  def apply(nc: NamesConfig): NamesConfigJs = {
    new NamesConfigJs(
      nc.relay,
      nc.getDataSrv,
      nc.callbackSrv,
      nc.responseSrv,
      nc.responseFnName,
      nc.errorHandlingSrv,
      nc.errorFnName
    )
  }
}

type LogLevel = "trace" | "debug" | "info" | "warn" | "error" | "off"

class Debug(val printParticleId: js.UndefOr[Boolean], val marineLogLevel: js.UndefOr[LogLevel]) extends js.Object
