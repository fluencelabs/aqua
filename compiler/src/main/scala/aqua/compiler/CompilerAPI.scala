package aqua.compiler

import aqua.compiler.AquaError.*
import aqua.backend.{AirFunction, Backend}
import aqua.linker.{AquaModule, Linker, Modules}
import aqua.model.AquaContext
import aqua.parser.lift.{LiftParser, Span}
import aqua.parser.{Ast, ParserError}
import aqua.raw.RawPart.Parts
import aqua.raw.{RawContext, RawPart}
import aqua.res.AquaRes
import aqua.semantics.header.{HeaderHandler, HeaderSem}
import aqua.semantics.{CompilerState, RawSemantics, Semantics}

import cats.data.*
import cats.data.Validated.{invalid, validNec, Invalid, Valid}
import cats.parse.Parser0
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.semigroup.*
import cats.syntax.traverse.*
import cats.syntax.either.*
import cats.{~>, Comonad, Monad, Monoid, Order}
import scribe.Logging

import scala.collection.MapView

object CompilerAPI extends Logging {

  private def toAquaProcessed[I: Order, E, S[_]: Comonad](
    filesWithContext: Map[I, RawContext]
  ): Chain[AquaProcessed[I]] = {
    logger.trace("linking finished")

    filesWithContext.toList
      // Process all contexts maintaining Cache
      .traverse { case (i, rawContext) =>
        for {
          cache <- State.get[AquaContext.Cache]
          _ = logger.trace(s"Going to prepare exports for $i...")
          (exp, expCache) = AquaContext.exportsFromRaw(rawContext, cache)
          _ = logger.trace(s"AquaProcessed prepared for $i")
          _ <- State.set(expCache)
        } yield AquaProcessed(i, exp)
      }
      .runA(AquaContext.Cache())
      // Convert result List to Chain
      .map(Chain.fromSeq)
      .value
  }

  private def getAquaCompiler[F[_]: Monad, E, I: Order, S[_]: Comonad](
    config: AquaCompilerConf
  ): AquaCompiler[F, E, I, S, RawContext] = {
    implicit val rc: Monoid[RawContext] = RawContext
      .implicits(
        RawContext.blank
          .copy(parts = Chain.fromSeq(config.constantsList).map(const => RawContext.blank -> const))
      )
      .rawContextMonoid

    val semantics = new RawSemantics[S]()

    new AquaCompiler[F, E, I, S, RawContext](new HeaderHandler[S, RawContext](), semantics)
  }

  // Get result generated by backend
  def compile[F[_]: Monad, E, I: Order, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    airValidator: AirValidator[F],
    backend: Backend.Transform,
    config: AquaCompilerConf
  ): F[ValidatedNec[AquaError[I, E, S], Chain[AquaCompiled[I]]]] = {
    val compiler = getAquaCompiler[F, E, I, S](config)

    for {
      compiledRaw <- compiler.compileRaw(sources, parser)
      compiledV = compiledRaw.value.value.toValidated.map(toAquaProcessed)
      _ <- airValidator.init()
      result <- compiledV.traverse { compiled =>
        compiled.traverse { ap =>
          logger.trace("generating output...")
          val res = backend.transform(ap.context)
          val compiled = backend.generate(res)
          airValidator
            .validate(
              compiled.toList.flatMap(_.air)
            )
            .map(
              _.leftMap(errs => AirValidationError(errs): AquaError[I, E, S])
                .as(
                  AquaCompiled(ap.id, compiled, res.funcs.length.toInt, res.services.length.toInt)
                )
                .toValidatedNec
            )
        }.map(_.sequence)
      }.map(_.andThen(identity)) // There is no flatTraverse for Validated
    } yield result
  }

  def compileTo[F[_]: Monad, E, I: Order, S[_]: Comonad, T](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    airValidator: AirValidator[F],
    backend: Backend.Transform,
    config: AquaCompilerConf,
    write: AquaCompiled[I] => F[Seq[Validated[E, T]]]
  ): F[ValidatedNec[AquaError[I, E, S], Chain[T]]] =
    compile[F, E, I, S](sources, parser, airValidator, backend, config)
      .flatMap(
        _.traverse(compiled =>
          compiled.toList.flatTraverse { ac =>
            write(ac).map(
              _.toList.map(
                _.bimap(
                  e => OutputError(ac, e): AquaError[I, E, S],
                  Chain.one
                ).toValidatedNec
              )
            )
          }.map(_.foldA)
        ).map(_.andThen(identity)) // There is no flatTraverse for Validated
      )

  def compileToContext[F[_]: Monad, E, I: Order, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    config: AquaCompilerConf
  ): F[ValidatedNec[AquaError[I, E, S], Chain[AquaContext]]] = {

    val compiler = getAquaCompiler[F, E, I, S](config)
    val compiledRaw = compiler.compileRaw(sources, parser)

    compiledRaw.map(
      _.value.value.toValidated
        .map(toAquaProcessed)
        .map(_.map { ap =>
          logger.trace("generating output...")
          ap.context
        })
    )
  }
}
