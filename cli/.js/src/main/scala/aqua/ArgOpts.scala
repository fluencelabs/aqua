package aqua

import aqua.builder.ArgumentGetter
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import Validated.{invalid, invalidNec, valid, validNec, validNel}
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.{LiteralToken, VarToken}
import aqua.parser.lift.Span
import aqua.types.{BottomType, LiteralType}
import cats.{~>, Id}
import cats.effect.Concurrent
import com.monovore.decline.Opts
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import fs2.io.file.Files

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
      .option[String]("func", "Function to call with args", "f")
      .mapValidated { str =>
        CallArrowExpr.funcOnly.parseAll(str) match {
          case Right(exprSpan) =>
            val expr = exprSpan.mapK(spanToId)

            val args = expr.args.collect {
              case LiteralToken(value, ts) =>
                LiteralRaw(value, ts)
              case VarToken(name, _) =>
                // TODO why BottomType?
                VarRaw(name.value, BottomType)
            }

            validNel(CliFunc(expr.funcName.value, args, expr.ability.map(_.name)))

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
          checkDataGetServices(func.args, data).map { getters =>
            FuncWithData(func, getters)
          }
        }
      }
    }
  }

  // checks if data is presented if there is non-literals in function arguments
  // creates services to add this data into a call
  def checkDataGetServices(
    args: List[ValueRaw],
    data: Option[js.Dynamic]
  ): ValidatedNec[String, Map[String, ArgumentGetter]] = {
    val vars = args.collect { case v @ VarRaw(_, _) =>
      v
    // one variable could be used multiple times
    }.distinctBy(_.name)

    data match {
      case None if vars.nonEmpty =>
        invalidNec("Function have non-literals, so, data should be presented")
      case None =>
        validNec(Map.empty)
      case Some(data) =>
        val services = vars.map { vm =>
          val arg = data.selectDynamic(vm.name)
          vm.name -> ArgumentGetter(vm, arg)
        }
        validNec(services.toMap)
    }
  }

  def dataOpt: Opts[js.Dynamic] =
    Opts.option[String]("data", "Argument map for aqua function in JSON format", "d").mapValidated {
      str =>
        Validated.catchNonFatal {
          JSON.parse(str)
        }.leftMap(t => NonEmptyList.one("Data isn't a valid JSON: " + t.getMessage))
    }

  def dataFromFileOpt[F[_]: Files: Concurrent]: Opts[F[ValidatedNec[String, js.Dynamic]]] = {
    FileOpts.fileOpt(
      "data-path",
      "Path to file with arguments map in JSON format",
      "p",
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
        invalidNec("Pass either data argument or path to file with arguments")
      case _ => validNec(dataFromArgument.orElse(dataFromFile))
    }
  }
}
