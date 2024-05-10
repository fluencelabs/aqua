package aqua.compiler

import aqua.backend.Backend
import aqua.compiler.AquaError.*
import aqua.model.AquaContext
import aqua.parser.{Ast, ParserError}
import aqua.raw.{ConstantRaw, RawContext}
import aqua.semantics.header.{HeaderHandler, HeaderSem}
import aqua.semantics.rules.locations.{DummyLocationsInterpreter, LocationsAlgebra}
import aqua.semantics.{FileId, RawSemantics}

import cats.data.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Comonad, Monad, Monoid, Order, Show}
import scribe.Logging

object CompilerAPI extends Logging {

  private def toAquaProcessed[I: FileId, S[_]: Comonad](
    filesWithContext: Map[I, RawContext]
  ): Chain[AquaProcessed[I]] = {
    logger.trace("linking finished")

    filesWithContext.toList
      // Process all contexts maintaining Cache
      .traverse { case (i, rawContext) =>
        AquaContext
          .exportsFromRaw(rawContext)
          .map(exp => AquaProcessed(i, exp))
      }
      .runA(AquaContext.Cache.empty)
      // Convert result List to Chain
      .map(Chain.fromSeq)
      .value
  }

  private def getAquaCompiler[F[_]: Monad, E, I: FileId, S[_]: Comonad](
    config: AquaCompilerConf
  ): AquaCompiler[F, E, I, S, RawContext] = {
    given LocationsAlgebra[S, State[RawContext, *]] =
      DummyLocationsInterpreter()

    val constants = config.constants ++ ConstantRaw.defaultConstants(config.relayVarName)

    new AquaCompiler(
      new HeaderHandler(),
      new RawSemantics(constants)
    )
  }

  // Get result generated by backend
  def compile[F[_]: Monad, E, I: FileId, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    airValidator: AirValidator[F],
    backend: Backend.Transform,
    config: AquaCompilerConf
  ): F[CompileResult[I, E, S][Chain[AquaCompiled[I]]]] = {
    val compiler = getAquaCompiler[F, E, I, S](config)

    for {
      compiledRaw <- compiler.compileRaw(sources, parser)
      compiledV = compiledRaw.map(toAquaProcessed)
      _ <- airValidator.init()
      result <- compiledV.flatTraverse { compiled =>
        compiled.traverse { ap =>
          logger.trace("generating output...")
          val res = backend.transform(ap.context)
          val generated = backend.generate(res)
          val air = generated.toList.flatMap(_.air)
          val compiled = AquaCompiled(
            sourceId = ap.id,
            compiled = generated,
            funcsCount = res.funcs.length.toInt,
            servicesCount = res.services.length.toInt
          )

          airValidator
            .validate(air)
            .map(
              _.leftMap(errs => AirValidationError(errs): AquaError[I, E, S])
                .as(compiled)
                .toValidatedNec
            )
        }.map(_.sequence.toEither.toEitherT)
      }
    } yield result
  }

  def compileToContext[F[_]: Monad, E, I: FileId, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    config: AquaCompilerConf
  ): F[CompileResult[I, E, S][Chain[AquaContext]]] = {

    val compiler = getAquaCompiler[F, E, I, S](config)
    val compiledRaw = compiler.compileRaw(sources, parser)

    compiledRaw.map(
      _.map(toAquaProcessed)
        .map(_.map { ap =>
          logger.trace("generating output...")
          ap.context
        })
    )
  }
}
