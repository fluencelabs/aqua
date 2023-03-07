package aqua.compiler

import aqua.AquaIO
import org.graalvm.nativeimage.IsolateThread
import org.graalvm.nativeimage.c.function.CEntryPoint
import aqua.io.AquaFileError
import aqua.files.{AquaFileSources, FileModuleId}
import cats.{Functor, Monad, Applicative}
import cats.data.{ValidatedNec, Chain}
import cats.data.Validated.{Valid}
import fs2.io.file.{Files, Path}
import cats.implicits.*

object Library {


  private final val path = Path("")

  private class RawAquaSource[F[_] : AquaIO : Monad : Files](input: String, imports: List[String]) extends AquaFileSources[F](path, imports.map(Path.apply)) {
    override def sources: F[ValidatedNec[AquaFileError, Chain[(FileModuleId, String)]]] = {
      Applicative[F].pure(Valid(Chain.one((FileModuleId(path), input))))
    }
  }

  private def compileF[F[_] : AquaIO : Monad : Files](input: String, imports: List[String]): F[String] = for {
    sources <- new RawAquaSource[F](input, imports).pure
    result <- CompilerAPI
      .compileTo[F, AquaFileError, FileModuleId, FileSpan.F, String](
        sources,
        SpanParser.parser,
        validator,
        new Backend.Transform:
          override def transform(ex: AquaContext): AquaRes =
            Transform.contextRes(ex, transformConfig)

          override def generate(aqua: AquaRes): Seq[Generated] = backend.generate(aqua)
        ,
        AquaCompilerConf(transformConfig.constantsList),
        targetPath.map(sources.write).getOrElse(dry[F])
      )
  } yield ()

  @CEntryPoint(name = "compile") def compile(thread: IsolateThread, code: String): Int = {
    1
  }

}
