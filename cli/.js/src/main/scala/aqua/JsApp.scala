package aqua

import aqua.backend.Backend
import aqua.backend.ts.TypeScriptBackend
import aqua.compiler.{AquaCompiler, AquaError, AquaSources}
import aqua.model.transform.TransformConfig
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.data.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.{Applicative, Monad, Show}
import aqua.parser.lift.FileSpan

object JsApp {

  trait AquaJsError {}

  case class FileId(name: String)

  class Sources[F[_]: Applicative] extends AquaSources[F, AquaJsError, FileId] {

    def sources: F[ValidatedNec[AquaJsError, Chain[(FileId, String)]]] =
      Validated.Valid(Chain.empty).pure[F]

    // Resolve id of the imported imp string from I file
    def resolveImport(from: FileId, imp: String): F[ValidatedNec[AquaJsError, FileId]] =
      Validated.Valid(FileId("")).pure[F]

    // Load file by its resolved I
    def load(file: FileId): F[ValidatedNec[AquaJsError, String]] = Validated.Valid("").pure[F]
  }

  def main(args: Array[String]): Unit = {
    // test code
//    val sources = new Sources[IO]()
//    val bodyConfig = GenerationConfig()
//    val b = AquaCompiler
//      .compileTo[IO, AquaJsError, FileId, FileSpan.F, String](
//        sources,
//        (fmid, src) => FileSpan.fileSpanLiftParser(fmid.name, src),
//        TypeScriptBackend,
//        bodyConfig,
//        (a) => ???
//      )
    println("Hello world!")
  }
}
