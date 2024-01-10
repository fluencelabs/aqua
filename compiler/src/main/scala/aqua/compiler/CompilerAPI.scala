package aqua.compiler

import aqua.backend.Backend
import aqua.compiler.AquaError.*
import aqua.model.AquaContext
import aqua.parser.{Ast, ParserError}
import aqua.raw.RawContext
import aqua.semantics.RawSemantics
import aqua.semantics.header.{HeaderHandler, HeaderSem}

import cats.data.*
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Comonad, Monad, Monoid, Order}
import scribe.Logging

object CompilerAPI extends Logging {

  private def toAquaProcessed[I: Order, E, S[_]: Comonad](
    filesWithContext: Map[I, RawContext]
  ): Chain[AquaProcessed[I]] = {
    logger.trace("linking finished")

    println("files with context: " + filesWithContext.keys)

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
      .run(AquaContext.Cache())
      .map { case (cache, a) =>
        println(
          cache.data.toList
            .flatMap(_._2.funcs.keys)
            .groupBy(identity)
            .view
            .values
            .map(l => (l.headOption, l.size))
            .toList.sortBy(_._2).reverse
        )
        println(
          cache.data.toList
            .flatMap(_._1.parts.map(_._2.name).toList)
            .groupBy(identity)
            .view
            .values
            .map(l => (l.headOption, l.size))
            .toList.sortBy(_._2).reverse
        )
        println("modules rawcontext: ")
        println(
          cache.data.toList
            .flatMap(_._1.module.toList)
            .groupBy(identity)
            .view
            .values
            .map(l => (l.headOption, l.size))
            .toList.sortBy(_._2).reverse
        )
        println("modules aquacontext: ")
        println(
          cache.data.toList
            .flatMap(_._2.module.toList)
            .groupBy(identity)
            .view
            .values
            .map(l => (l.headOption, l.size))
            .toList.sortBy(_._2).reverse
        )
        a
      }
      // Convert result List to Chain
      .map(a => Chain.fromSeq(a))
      .value
  }

  private def getAquaCompiler[F[_]: Monad, E, I: Order, S[_]: Comonad](
    config: AquaCompilerConf
  ): AquaCompiler[F, E, I, S, RawContext] = {
    given Monoid[RawContext] = RawContext
      .implicits(
        RawContext.blank.copy(
          parts = Chain
            .fromSeq(config.constantsList)
            .map(const => RawContext.blank -> const)
        )
      )
      .rawContextMonoid

    val semantics = new RawSemantics[S]()

    new AquaCompiler[F, E, I, S, RawContext](
      new HeaderHandler(),
      semantics
    )
  }

  // Get result generated by backend
  def compile[F[_]: Monad, E, I: Order, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    airValidator: AirValidator[F],
    backend: Backend.Transform,
    config: AquaCompilerConf
  ): F[CompileResult[I, E, S][Chain[AquaCompiled[I]]]] = {
    val compiler = getAquaCompiler[F, E, I, S](config)

    for {
      _ <- logger.trace("Start compilation").pure[F]
      compiledRaw <- compiler.compileRaw(sources, parser)
      _ <- logger.trace("Compile raw ended").pure[F]
      compiledV = compiledRaw.map(toAquaProcessed)
      _ <- logger.trace("Aqua processed ended").pure[F]
      _ <- airValidator.init()
      result <- compiledV.flatTraverse { compiled =>
        compiled.traverse { ap =>
          logger.trace("generating output...")
          val res = backend.transform(ap.context)
          logger.trace("end transforming...")
          val generated = backend.generate(res)
          logger.trace("end generating...")
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
      _ <- logger.trace("Writing result ended").pure[F]
    } yield result
  }

  def compileToContext[F[_]: Monad, E, I: Order, S[_]: Comonad](
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
