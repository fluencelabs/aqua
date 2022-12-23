package aqua.run

import aqua.ArgOpts.jsonFromFileOpts
import aqua.builder.{AquaFunction, ArgumentGetter, Service}
import aqua.definitions.{ArrowTypeDef, ProductTypeDef, TypeDefinition}
import aqua.js.{Conversions, ServiceHandler, TypeDefinitionJs}
import aqua.json.JsonEncoder
import aqua.model.{AquaContext, ServiceModel}
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.{CallArrowToken, CollectionToken, LiteralToken, VarToken}
import aqua.parser.lift.Span
import aqua.raw.value.{CollectionRaw, LiteralRaw, ValueRaw, VarRaw}
import aqua.types.*
import cats.data.*
import cats.data.Validated.{invalid, invalidNec, invalidNel, valid, validNec, validNel}
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.semigroup.*
import cats.syntax.traverse.*
import cats.{Id, Semigroup, ~>}
import com.monovore.decline.Opts
import fs2.io.file.{Files, Path}

import scala.scalajs.js

// Description of a service with functions that return structures
case class JsonService(name: String, serviceId: String, functions: NonEmptyList[JsonFunction])
case class JsonFunction(name: String, result: js.Dynamic, resultType: Type)

object JsonService {

  def findServices(
    contexts: Chain[AquaContext],
    services: List[JsonService]
  ): ValidatedNec[String, List[Service]] = {
    services
      .map(js =>
        contexts
          .collectFirstSome(_.services.get(js.name))
          .map(sm => (js, sm))
          .map(validNec)
          .getOrElse(
            Validated.invalidNec[String, ServiceModel](
              s"There is no service '${js.name}' (described in json-service file) in aqua source or it is not exported. Check the spelling or see https://fluence.dev/docs/aqua-book/language/header/#export"
            )
          )
      )
      .sequence
      .andThen { l =>
        l.map { case (jsonService: JsonService, sm: ServiceModel) =>
          val aquaFunctions: ValidatedNec[String, NonEmptyList[AquaFunction]] =
            jsonService.functions.map { jf =>
              sm.arrows(jf.name)
                .map { case arr: ArrowType =>
                  if (arr.domain.isEmpty)
                    TypeValidator
                      .validateTypes(jf.name, arr.codomain, Some(ProductType(jf.resultType :: Nil)))
                      .map { _ =>
                        new AquaFunction {
                          override def fnName: String = jf.name

                          override def handler: ServiceHandler = _ => {
                            val converted = arr.codomain.toList match {
                              case h :: _ =>
                                Conversions.ts2aqua(jf.result, TypeDefinitionJs(TypeDefinition(h)))
                              case Nil =>
                                Conversions.ts2aqua(
                                  jf.result,
                                  TypeDefinitionJs(TypeDefinition(NilType))
                                )
                            }

                            js.Promise.resolve(converted)
                          }
                          override def arrow: ArrowTypeDef =
                            ArrowTypeDef(ProductTypeDef(NilType), ProductTypeDef(arr.codomain))
                        }
                      }
                  else
                    invalidNec(s"Json service '${jf.name}' cannot have any arguments")
                }
                .getOrElse(
                  Validated.invalidNec[String, AquaFunction](
                    s"There is no function '${jf.name}' in service '${jsonService.name}' in aqua source. Check your 'json-service' options"
                  )
                )
            }.sequence

          aquaFunctions.map(funcs => Service(jsonService.serviceId, funcs))
        }.sequence
      }
  }

}
