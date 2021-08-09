package aqua

import aqua.backend.Backend
import aqua.backend.ts.TypeScriptBackend
import aqua.compiler.{AquaCompiler, AquaError, AquaSources}
import aqua.model.transform.GenerationConfig
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.data.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.{Monad, Show}
import aqua.parser.lift.FileSpan

object JsApp {

  trait AquaJsError {}

  case class FileId(name: String)

  class Sources[F[_]] extends AquaSources[F, AquaJsError, FileId] {
    def sources: F[ValidatedNec[AquaJsError, Chain[(FileId, String)]]] = ???

    // Resolve id of the imported imp string from I file
    def resolveImport(from: FileId, imp: String): F[ValidatedNec[AquaJsError, FileId]] = ???

    // Load file by its resolved I
    def load(file: FileId): F[ValidatedNec[AquaJsError, String]] = ???
  }

  def main(args: Array[String]): Unit = {
    val sources = new Sources[IO]()
    val bodyConfig = GenerationConfig()
    val b = AquaCompiler
      .compileTo[IO, AquaJsError, FileId, FileSpan.F, String](
        sources,
        (fmid, src) => FileSpan.fileSpanLiftParser(fmid.name, src),
        TypeScriptBackend,
        bodyConfig,
        (a) => ???
      )
    println("Hello world!")
  }
}
