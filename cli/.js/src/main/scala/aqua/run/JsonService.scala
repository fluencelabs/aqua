package aqua.run

import aqua.ArgOpts.jsonFromFileOpt
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
import cats.{~>, Id, Semigroup}
import com.monovore.decline.Opts
import fs2.io.file.Files

import scala.scalajs.js

case class JsonFunction(name: String, result: js.Dynamic, resultType: Type)
case class JsonService(name: String, functions: NonEmptyList[JsonFunction])

object JsonService {

  def jsonServiceOpt[F[_]: Files: Concurrent]: Opts[F[ValidatedNec[String, JsonService]]] = {
    jsonFromFileOpt("json-service", "Path to file that describes service with JSON result", "j")
      .map(b =>
        b.map { case a: ValidatedNec[String, js.Dynamic] =>
          a.andThen { res =>
            val name = res.name
            val functionsRaw = res.functions
            if (js.isUndefined(name) || js.typeOf(name) != "string")
              invalidNec("No name in JSON service or it is not a string")
            else if (js.isUndefined(functionsRaw) || !js.Array.isArray(functionsRaw))
              invalidNec("'functions' field should exists and be an array in JSON service")
            else {
              val functionsV: ValidatedNec[String, List[JsonFunction]] = functionsRaw
                .asInstanceOf[js.Array[js.Dynamic]]
                .toList
                .map { f =>
                  val fName = f.name
                  val fResult = f.result
                  if (js.isUndefined(fName) || js.typeOf(fName) != "string")
                    invalidNec("One of a functions don't have a name or it is not a string")
                  else if (js.isUndefined(fResult))
                    invalidNec(s"Function '$fName' don't have a result")
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
                  .map(fNEL => validNec(JsonService(name.asInstanceOf[String], fNEL)))
                  .getOrElse(invalidNec(s"List of functions in '$name' service is empty"))
              }
            }

          }
        }
      )
  }
}
