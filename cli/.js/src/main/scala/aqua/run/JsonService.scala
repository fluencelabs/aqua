package aqua.run

import aqua.ArgOpts.jsonFromFileOpts
import aqua.builder.ArgumentGetter
import aqua.json.JsonEncoder
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

case class JsonFunction(name: String, result: js.Dynamic, resultType: Type)
case class JsonService(name: String, serviceId: String, functions: NonEmptyList[JsonFunction])

object JsonService {

  def jsonServiceOpt[F[_]: Files: Concurrent]: Opts[F[ValidatedNec[String, NonEmptyList[JsonService]]]] = {
    jsonFromFileOpts("json-service", "Path to file that describes service with JSON result", "j")
      .map(b =>
        b.map { case a: ValidatedNec[String, NonEmptyList[(Path, js.Dynamic)]] =>
          a.andThen { results =>
            results.map { case (path, res) =>
              val name = res.name
              val serviceId = res.serviceId
              val functionsRaw = res.functions
              if (js.isUndefined(name) || js.typeOf(name) != "string")
                invalidNec(s"No name in JSON service '$path' or it is not a string")
              else if (js.isUndefined(serviceId) || js.typeOf(serviceId) != "string")
                invalidNec(s"No serviceId in JSON service '$path' or it is not a string")
              else if (js.isUndefined(functionsRaw) || !js.Array.isArray(functionsRaw))
                invalidNec(s"'functions' field should exists and be an array in JSON service '$path'")
              else {
                val functionsV: ValidatedNec[String, List[JsonFunction]] = functionsRaw
                  .asInstanceOf[js.Array[js.Dynamic]]
                  .toList
                  .map { f =>
                    val fName = f.name
                    val fResult = f.result
                    if (js.isUndefined(fName) || js.typeOf(fName) != "string")
                      invalidNec(s"One of a functions don't have a name or it is not a string in JSON service $path")
                    else if (js.isUndefined(fResult))
                      invalidNec(s"Function '$fName' don't have a result in $path")
                    else {
                      val funcName = fName.asInstanceOf[String]
                      JsonEncoder
                        .aquaTypeFromJson(funcName, fResult)
                        .map(t => JsonFunction(funcName, fResult, t))
                    }
                  }
                  .sequence

                functionsV.andThen { fs =>
                  NonEmptyList
                    .fromList(fs)
                    .map(fNEL =>
                      validNec(
                        JsonService(name.asInstanceOf[String], serviceId.asInstanceOf[String], fNEL)
                      )
                    )
                    .getOrElse(invalidNec(s"List of functions in '$name' service is empty in $path"))
                }
              }
            }.sequence
          }
        }
      )

  }
}
