package aqua

import aqua.builder.ArgumentGetter
import aqua.json.JsonEncoder
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.{CallArrowToken, CollectionToken, LiteralToken, VarToken}
import aqua.parser.lift.Span
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.types.*
import cats.data.Validated.{invalid, invalidNec, invalidNel, valid, validNec, validNel}
import cats.data.*
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.semigroup.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{~>, Id, Semigroup}
import com.monovore.decline.Opts
import fs2.io.file.Files

import scala.collection.immutable.SortedMap
import scala.scalajs.js
import scala.scalajs.js.JSON

case class FuncWithData(func: CliFunc, getters: Map[String, ArgumentGetter])
case class CliFunc(name: String, args: List[ValueRaw] = Nil, ability: Option[String] = None)

object ArgOpts {

  def spanToId: Span.S ~> Id = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): Id[A] = {
      span._2
    }
  }

  // Parses a function name and arguments from a string
  def funcOpt: Opts[CliFunc] =
    Opts
      .option[String]("func", "Function to call with args", "f", "funcName(args)")
      .mapValidated { str =>
        CallArrowToken.callArrow.parseAll(str) match {
          case Right(exprSpan) =>
            val expr = exprSpan.mapK(spanToId)

            val argsV = expr.args.collect {
              case LiteralToken(value, ts) =>
                validNel(LiteralRaw(value, ts))
              case VarToken(name, _) =>
                validNel(VarRaw(name.value, BottomType))
              case CollectionToken(_, _) =>
                invalidNel("Array arguments are currently not supported. \nYou can provide JSON arguments via --data or --data-path")
              case CallArrowToken(_, _, _) =>
                invalidNel("Function calls as arguments are not supported.")
            }.sequence
            argsV.andThen(args =>
              validNel(CliFunc(expr.funcName.value, args, expr.ability.map(_.name)))
            )

          case Left(err) => invalid(err.expected.map(_.context.mkString("\n")))
        }
      }

  // Gets data from a file or from a json string
  def dataFileOrStringOpt[F[_]: Files: Concurrent]
    : Opts[F[ValidatedNec[String, Option[js.Dynamic]]]] =
    (AppOpts.wrapWithOption(dataOpt), AppOpts.wrapWithOption(dataFromFileOpt[F])).mapN {
      case (dataFromString, dataFromFile) =>
        dataFromFile match {
          case Some(dataFromFileF) =>
            dataFromFileF.map(_.andThen(args => getData(Some(args), dataFromString)))
          case None => validNec(dataFromString).pure[F]
        }
    }

  // Creates getters based on function arguments and data, return all info
  def funcWithArgsOpt[F[_]: Files: Concurrent]: Opts[F[ValidatedNec[String, FuncWithData]]] = {
    (dataFileOrStringOpt[F], funcOpt).mapN { case (dataF, func) =>
      dataF.map { dataV =>
        dataV.andThen { data =>
          checkDataGetServices(func, data).map { case (funcWithTypedArgs, getters) =>
            FuncWithData(funcWithTypedArgs, getters)
          }
        }
      }
    }
  }

  // checks if data is presented if there is non-literals in function arguments
  // creates services to add this data into a call
  def checkDataGetServices(
    cliFunc: CliFunc,
    data: Option[js.Dynamic]
  ): ValidatedNec[String, (CliFunc, Map[String, ArgumentGetter])] = {
    val vars = cliFunc.args.collect { case v @ VarRaw(_, _) =>
      v
    // one variable could be used multiple times
    }.distinctBy(_.name)

    data match {
      case None if vars.nonEmpty =>
        // TODO: add a list  with actual argument names that where present in the function call
        invalidNec("Undefined variables. You can provide them via --data or --data-path flags")
      case None =>
        validNec((cliFunc, Map.empty))
      case Some(data) =>
        vars.map { vm =>
          val arg = {
            val a = data.selectDynamic(vm.name)
            if (js.isUndefined(a)) null
            else a
          }

          val typeV = JsonEncoder.aquaTypeFromJson(vm.name, arg)

          typeV.map(t => (vm.copy(baseType = t), arg))
        }.sequence
          .map(_.map { case (vm, arg) =>
            vm.name -> ArgumentGetter(vm, arg)
          }.toMap)
          .andThen { services =>
            val argsWithTypes = cliFunc.args.map {
              case v @ VarRaw(n, t) =>
                // argument getters have been enriched with types derived from JSON
                // put this types to unriched arguments in CliFunc
                services.get(n).map(g => v.copy(baseType = g.function.value.baseType)).getOrElse(v)
              case v => v
            }

            validNec((cliFunc.copy(args = argsWithTypes), services))
          }
    }
  }

  def dataOpt: Opts[js.Dynamic] =
    Opts
      .option[String]("data", "JSON in { [argumentName]: argumentValue } format. You can call a function using these argument names", "d", "json")
      .mapValidated { str =>
        Validated.catchNonFatal {
          JSON.parse(str)
        }.leftMap(t => NonEmptyList.one("Data argument isn't a valid JSON: " + t.getMessage))
      }

  def dataFromFileOpt[F[_]: Files: Concurrent]: Opts[F[ValidatedNec[String, js.Dynamic]]] = {
    jsonFromFileOpt("data-path", "Path to a JSON file in { [argumentName]: argumentValue } format. You can call a function using these argument names", "p")
  }

  def jsonFromFileOpt[F[_]: Files: Concurrent](
    name: String,
    help: String,
    short: String
  ): Opts[F[ValidatedNec[String, js.Dynamic]]] = {
    FileOpts.fileOpt(
      name,
      help,
      short,
      (path, str) => {
        Validated.catchNonFatal {
          JSON.parse(str)
        }.leftMap(t =>
          NonEmptyChain
            .one(s"Data in ${path.toString} isn't a valid JSON: " + t.getMessage)
        )
      }
    )
  }

  // get data from sources, error if both sources exist
  def getData(
    dataFromArgument: Option[js.Dynamic],
    dataFromFile: Option[js.Dynamic]
  ): ValidatedNec[String, Option[js.Dynamic]] = {
    (dataFromArgument, dataFromFile) match {
      case (Some(_), Some(_)) =>
        // TODO: maybe allow to use both and simple merge with data argument having higher priority
        invalidNec("Please use either --data or --data-path. Don't use both")
      case _ => validNec(dataFromArgument.orElse(dataFromFile))
    }
  }
}
