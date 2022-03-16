package aqua.dist

import aqua.builder.{ArgumentGetter, IPFSUploader}
import aqua.files.AquaFilesIO
import aqua.io.OutputPrinter
import aqua.ipfs.js.IpfsApi
import aqua.js.{Config, Fluence, PeerConfig}
import aqua.keypair.KeyPairShow.show
import aqua.model.LiteralModel
import aqua.raw.value.{LiteralRaw, VarRaw}
import aqua.run.RunCommand.createKeyPair
import aqua.run.{GeneralRunOptions, RunCommand, RunConfig, RunOpts}
import aqua.*
import aqua.ArgOpts.jsonFromFileOpt
import aqua.run.RunOpts.logger
import aqua.types.{ArrayType, ScalarType, StructType}
import cats.data.Validated.{invalid, invalidNec, valid, validNec, validNel}
import cats.data.*
import cats.effect.kernel.Async
import cats.effect.{Concurrent, ExitCode, Resource, Sync}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.show.*
import cats.{Applicative, Monad}
import com.monovore.decline.{Command, Opts}
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.collection.immutable.SortedMap
import scala.scalajs.js.JSConverters.*
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js

// Options and commands to work with IPFS
object DistOpts extends Logging {

  val DistAqua = "aqua/dist.aqua"

  val DeployFuncName = "deploy"
  val RemoveFuncName = "remove"
  val CreateServiceFuncName = "createService"
  val AddBlueprintFuncName = "addBlueprint"

  def srvNameOpt: Opts[String] =
    Opts
      .option[String]("service", "What service from the config file to deploy", "s")

  def srvIdOpt: Opts[String] =
    Opts
      .option[String]("id", "Service id to remove", "i")

  def blueprintIdOpt: Opts[String] =
    Opts
      .option[String]("id", "Blueprint id", "i")

  def blueprintNameOpt: Opts[String] =
    Opts
      .option[String]("name", "Blueprint name", "n")

  def dependencyOpt: Opts[NonEmptyList[String]] =
    Opts
      .options[String]("dependency", "Blueprint dependency. May be used several times", "d")

  def fillConfigOptionalFields(getter: ArgumentGetter): ArgumentGetter = {
    val arg = getter.function.arg
    val filledConfig = Config.fillWithEmptyArrays(arg)
    ArgumentGetter(getter.function.value, filledConfig)
  }

  // Removes service from a node
  def remove[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.valid(
      "remove",
      "Remove a service from a remote peer",
      (GeneralRunOptions.commonOpt, srvIdOpt).mapN { (common, srvId) =>
        RunInfo(
          common,
          CliFunc(RemoveFuncName, LiteralRaw.quote(srvId) :: Nil),
          PackagePath(DistAqua)
        )
      }
    )

  def createService[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.valid(
      "create_service",
      "Create a service",
      (GeneralRunOptions.commonOpt, blueprintIdOpt).mapN { (common, blueprintId) =>
        RunInfo(
          common,
          CliFunc(CreateServiceFuncName, LiteralRaw.quote(blueprintId) :: Nil),
          PackagePath(DistAqua)
        )
      }
    )

  def addBlueprint[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.valid(
      "add_blueprint",
      "Add a blueprint to a node",
      (GeneralRunOptions.commonOpt, blueprintNameOpt, dependencyOpt).mapN {
        (common, blueprintName, dependencies) =>
          val depsWithHash = dependencies.map { d =>
            if (d.startsWith("hash:"))
              d
            else
              "hash:" + d
          }
          val addBlueprintType = StructType(
            "AddBlueprint",
            NonEmptyMap(
              ("name", ScalarType.string),
              SortedMap(("dependencies", ArrayType(ScalarType.string)))
            )
          )
          val addBlueprintRequestVar =
            VarRaw("addBlueprint", addBlueprintType)
          RunInfo(
            common,
            CliFunc(AddBlueprintFuncName, addBlueprintRequestVar :: Nil),
            PackagePath(DistAqua),
            Nil,
            Map(
              addBlueprintRequestVar.name -> ArgumentGetter(
                addBlueprintRequestVar,
                js.Dynamic
                  .literal("name" -> blueprintName, "dependencies" -> depsWithHash.toList.toJSArray)
              )
            )
          )
      }
    )

  def configFromFileOpt[F[_]: Files: Concurrent]: Opts[F[ValidatedNec[String, js.Dynamic]]] = {
    jsonFromFileOpt("config-path", "Path to a deploy config", "p")
  }

  // Uploads a file to IPFS, creates blueprints and deploys a service
  def deploy[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.applyF(
      "deploy",
      "Deploy a service onto a remote peer",
      (
        GeneralRunOptions.commonOpt,
        configFromFileOpt[F],
        srvNameOpt
      ).mapN { (common, dataFromFileF, srvName) =>
        dataFromFileF.map { dff =>
          val args = LiteralRaw.quote(srvName) :: VarRaw(srvName, ScalarType.string) :: Nil
          dff
            .andThen(data =>
              ArgOpts
                .checkDataGetServices(args, Some(data))
                .map(getServices =>
                  // if we have default timeout, increase it
                  val commonWithTimeout = if (common.timeout.isEmpty) {
                    common.copy(timeout = Some(60000))
                  } else common
                  RunInfo(
                    commonWithTimeout,
                    CliFunc(DeployFuncName, args),
                    PackagePath(DistAqua),
                    Nil,
                    // hack: air cannot use undefined fields, fill undefined arrays with nils
                    getServices.map { (k, v) => (k, fillConfigOptionalFields(v)) }
                  )
                )
            )
        }
      }
    )
}
