package aqua.compiler

import aqua.AquaIO
import org.graalvm.nativeimage.IsolateThread
import org.graalvm.nativeimage.c.function.CEntryPoint
import org.graalvm.nativeimage.c.`type`.CCharPointer
import org.graalvm.nativeimage.c.`type`.CTypeConversion
import aqua.io.AquaFileError
import aqua.files.{AquaFileSources, FileModuleId}
import cats.{Applicative, Functor, Monad}
import cats.data.{Chain, ValidatedNec}
import cats.data.Validated.Valid
import cats.data.Validated.validNec
import fs2.io.file.{Files, Path}
import cats.implicits.*
import aqua.SpanParser
import aqua.model.transform.TransformConfig
import aqua.model.AquaContext
import aqua.model.transform.Transform
import aqua.backend.Backend
import aqua.backend.AirFunction
import aqua.backend.Generated
import aqua.res.AquaRes
import aqua.parser.lift.FileSpan

object Library {


  private final val path = Path("")

  private class RawAquaSource[F[_] : AquaIO : Monad : Files](input: String, imports: List[String]) extends AquaFileSources[F](path, imports.map(Path.apply)):
    override def sources: F[ValidatedNec[AquaFileError, Chain[(FileModuleId, String)]]] = {
      Applicative[F].pure(Valid(Chain.one((FileModuleId(path), input))))
    }


  private class LocalBackendTransform(backend: Backend,
                                      transformConfig: TransformConfig) extends Backend.Transform:
    override def transform(ex: AquaContext): AquaRes =
      Transform.contextRes(ex, transformConfig)

    override def generate(aqua: AquaRes): Seq[Generated] = backend.generate(aqua)


  private class LocalAirValidator[F[_] : Applicative]() extends AirValidator[F]:

    override def init(): F[Unit] = Applicative[F].pure(())

    override def validate(airs: List[AirFunction]): F[ValidatedNec[String, Unit]] =
      Applicative[F].pure(validNec(()))

  private def compileF[F[_] : AquaIO : Monad : Files](input: String, imports: List[String]): F[ValidatedNec[AquaError[FileModuleId, AquaFileError, FileSpan.F], Chain[AquaCompiled[FileModuleId]]]] = for {
    sources <- new RawAquaSource[F](input, imports).pure
    backendTransform <- new LocalBackendTransform(???, ???).pure
    validator <- new LocalAirValidator[F]().pure
    transformConfig <- TransformConfig().pure
    result <- CompilerAPI
      .compile[F, AquaFileError, FileModuleId, FileSpan.F](
        sources,
        SpanParser.parser,
        validator,
        backendTransform,
        AquaCompilerConf(transformConfig.constantsList))
  } yield result

  @CEntryPoint(name = "compile") def compile(thread: IsolateThread, codePointer: CCharPointer): Int = {
    val code = CTypeConversion.toJavaString(codePointer)

    1
  }

}
