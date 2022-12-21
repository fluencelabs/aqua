package aqua.run.plugin

import aqua.js.{CallJsFunction, FluencePeer, ServiceHandler}
import aqua.run.JsonService
import aqua.run.plugin.Plugin.toPromise
import aqua.types.TopType
import aqua.definitions.*
import cats.data.{NonEmptyList, ValidatedNec}
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.data.Validated.{invalid, invalidNec, valid, validNec, validNel}
import com.monovore.decline.Opts
import fs2.io.file.{Files, Path}

import scalajs.js
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js.Promise

case class Function(name: String, closure: js.Function)

case class Plugin(name: String, functions: List[Function]) {

  def register(peer: FluencePeer): Unit = {
    val (handlers, funcTypes) = functions.map { f =>
      // get arguments types as TopType
      val argCount = f.closure.length
      val fields = Range(0, argCount).toList.map { i => ("arg" + i, TopTypeDef) }
      val arrowType =
        ArrowTypeDef(LabeledProductTypeDef(fields), UnlabeledProductTypeDef(TopTypeDef :: Nil))
      val fType = (f.name, arrowType)

      // handlers for registering
      val h: ServiceHandler = args => {
        val argsList = Range(0, argCount).toList.map { i =>
          args(i)
        }
        val res = f.closure.call(this.asInstanceOf[js.Any], argsList: _*)
        toPromise(res)

      }

      ((f.name, h), fType)
    }.unzip

    CallJsFunction.registerService(
      peer,
      name,
      handlers,
      ServiceDef(Some(name), LabeledProductTypeDef(funcTypes))
    )
  }
}

object Plugin {

  private def fileExt(p: Path): String =
    p.fileName.toString.split('.').toList.lastOption.getOrElse("")

  def pathToMjsFilesList[F[_]: Files: Concurrent](str: String): F[ValidatedNec[String, List[String]]] = {
    val path = Path(str).absolute
    Files[F]
      .exists(path)
      .flatMap { exists =>
        if (exists)
          Files[F].isRegularFile(path).flatMap { isFile =>
            if (isFile) {
              if (fileExt(path) == "mjs") {
                validNec(path.toString :: Nil).pure[F]
              } else {
                invalidNec(s"If path '$str' is a file, it must be with '.mjs' extension")
                  .pure[F]
              }
            } else {
              Files[F]
                .list(path)
                .evalMap { ps =>
                  val psAbs = ps.absolute
                  for {
                    isFile <- Files[F].isRegularFile(ps)
                    files <-
                      if (isFile) {
                        if (fileExt(ps) == "mjs") (psAbs :: Nil).pure[F]
                        else Nil.pure[F]
                      } else if (ps.fileName.toString != "node_modules") {
                        Files[F].list(psAbs).filter(pp => fileExt(pp) == "mjs").compile.toList
                      } else {
                        Nil.pure[F]
                      }
                  } yield {
                    files
                  }
                }
                .compile
                .toList
                .map(_.flatten.map(_.absolute.toString))
                .map(validNec)
            }
          }
        else {
          invalidNec(s"There is no path '$str'").pure[F]
        }
      }
  }

  def opt[F[_]: Files: Concurrent]: Opts[F[ValidatedNec[String, List[String]]]] = {
    Opts
      .options[String]("plugin", "[experimental] Path to a directory with JS plugins", "", "path")
      .map { strs =>
        strs.toList.map(s => pathToMjsFilesList(s)).sequence.map(_.sequence.map(_.flatten))
      }
  }

  def getPlugins(paths: List[String])(implicit
    ec: ExecutionContext
  ): Future[List[Plugin]] =
    paths.map(p => getPlugin(p)).sequence.map(_.flatten)

  private def toPromise(arg: js.Dynamic): js.Promise[js.Dynamic] = {
    if (js.typeOf(arg) == "object" && js.typeOf(arg.`then`) == "function")
      arg.asInstanceOf[js.Promise[js.Dynamic]]
    else js.Promise.resolve(arg)
  }

  def getPlugin(path: String)(implicit
    ec: ExecutionContext
  ): Future[List[Plugin]] = {
    for {
      file <- js.`import`[js.Dynamic](path).toFuture
      plugin <- {
        if (js.typeOf(file.plugins) == "function") {
          val res = file.applyDynamic("plugins")()
          toPromise(res).toFuture.map(_.asInstanceOf[js.Dictionary[js.Dictionary[js.Any]]])
        } else {
          Future(js.Dictionary[js.Dictionary[js.Any]]())
        }
      }
    } yield {
      plugin.map { case (k, v) =>
        val functions = v.map { case (kf, vf) =>
          Function(kf, vf.asInstanceOf[js.Function])
        }.toList
        Plugin(k, functions)
      }.toList
    }
  }
}
