package aqua.js

import aqua.*
import aqua.backend.*
import aqua.definitions.*
import aqua.res.FuncRes
import aqua.types.*

import io.circe.{Encoder, Json}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExportAll, JSImport}

@JSExportAll
case class FunctionDefJs(
  functionName: String,
  arrow: ArrowTypeDefJs,
  names: NamesConfigJs
)

object FunctionDefJs {

  def apply(fd: FunctionDef): FunctionDefJs = {
    FunctionDefJs(
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

@JSExportAll
sealed trait TypeDefinitionJs

@JSExportAll
case class ArrayTypeDefJs(`type`: TypeDefinitionJs, tag: String) extends TypeDefinitionJs

@JSExportAll
case class OptionTypeDefJs(`type`: TypeDefinitionJs, tag: String) extends TypeDefinitionJs

@JSExportAll
case class ScalarTypeDefJs(name: String, tag: String) extends TypeDefinitionJs

@JSExportAll
case class StructTypeDefJs(
  name: String,
  fields: js.Dictionary[TypeDefinitionJs],
  tag: String
) extends TypeDefinitionJs

@JSExportAll
case class LabeledTypeDefJs(fields: js.Dictionary[TypeDefinitionJs], tag: String)
    extends TypeDefinitionJs

object LabeledTypeDefJs {

  def apply(l: LabeledProductTypeDef): LabeledTypeDefJs = {

    LabeledTypeDefJs(
      js.Dictionary[TypeDefinitionJs](l.fields.map { case (n, t) => (n, TypeDefinitionJs(t)) }: _*),
      l.tag
    )
  }
}

@JSExportAll
case class UnlabeledTypeDefJs(items: js.Array[TypeDefinitionJs], tag: String)
    extends TypeDefinitionJs

@JSExportAll
case class TopTypeDefJs(tag: String) extends TypeDefinitionJs

@JSExportAll
case class BottomTypeDefJs(tag: String) extends TypeDefinitionJs

@JSExportAll
case class NilTypeDefJs(tag: String) extends TypeDefinitionJs

@JSExportAll
case class ArrowTypeDefJs(
  domain: TypeDefinitionJs,
  codomain: TypeDefinitionJs,
  tag: String
) extends TypeDefinitionJs

object TypeDefinitionJs {

  def apply(td: TypeDefinition): TypeDefinitionJs = td match {
    case o @ OptionTypeDef(t) => OptionTypeDefJs(apply(t), o.tag)
    case a @ ArrayTypeDef(t) => ArrayTypeDefJs(apply(t), a.tag)
    case s @ ScalarTypeDef(n) => ScalarTypeDefJs(n, s.tag)
    case s @ StructTypeDef(n, f) =>
      StructTypeDefJs(
        n,
        js.Dictionary[TypeDefinitionJs](f.toList.map { case (n, t) =>
          (n, TypeDefinitionJs(t))
        }: _*),
        s.tag
      )
    case l: LabeledProductTypeDef =>
      LabeledTypeDefJs(l)
    case u @ UnlabeledProductTypeDef(items) =>
      UnlabeledTypeDefJs(items.map(TypeDefinitionJs.apply).toJSArray, u.tag)
    case a @ ArrowTypeDef(domain, codomain) =>
      ArrowTypeDefJs(apply(domain), apply(codomain), a.tag)
    case n @ NilTypeDef => NilTypeDefJs(n.tag)
    case n @ TopTypeDef => TopTypeDefJs(n.tag)
    case n @ BottomTypeDef => BottomTypeDefJs(n.tag)
  }
}

@JSExportAll
case class ServiceDefJs(
  defaultServiceId: js.UndefOr[String],
  functions: LabeledTypeDefJs
)

object ServiceDefJs {

  def apply(sd: ServiceDef): ServiceDefJs = {
    ServiceDefJs(
      sd.defaultServiceId.getOrElse(()),
      LabeledTypeDefJs(sd.functions)
    )
  }
}

@JSExportAll
case class NamesConfigJs(
  relay: String,
  getDataSrv: String,
  callbackSrv: String,
  responseSrv: String,
  responseFnName: String,
  errorHandlingSrv: String,
  errorFnName: String
)

object NamesConfigJs {

  def apply(nc: NamesConfig): NamesConfigJs = {
    NamesConfigJs(
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

@JSExportAll
case class Debug(printParticleId: js.UndefOr[Boolean], marineLogLevel: js.UndefOr[LogLevel])
