package aqua.compiler

import aqua.backend.Backend
import aqua.linker.Linker
import aqua.model.AquaContext
import aqua.model.transform.TransformConfig
import aqua.model.transform.res.AquaRes
import aqua.parser.lift.LiftParser
import aqua.parser.Ast
import aqua.semantics.Semantics
import cats.data.Validated.{validNec, Invalid, Valid}
import cats.data.{Chain, NonEmptyChain, NonEmptyMap, Validated, ValidatedNec}
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.{Comonad, Monad, Monoid, Order}
import scribe.Logging

object AquaCompiler extends Logging {

  def compile[F[_]: Monad, E, I: Order, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    liftI: (I, String) => LiftParser[S],
    backend: Backend,
    config: TransformConfig
  ): F[ValidatedNec[AquaError[I, E, S], Chain[AquaCompiled[I]]]] = {
    import config.aquaContextMonoid
    type Err = AquaError[I, E, S]
    type Ctx = NonEmptyMap[I, AquaContext]
    type ValidatedCtx = ValidatedNec[Err, Ctx]

    // TODO factor out
    // TODO handle errors as ValidatedNec
    // TODO return prepare exports as well
    def prepareImports[S[_]](
      imports: Map[String, AquaContext],
      header: Ast.Head[S]
    ): ValidatedNec[Err, AquaContext] =
      validNec(imports.values.foldLeft(Monoid[AquaContext].empty)(Monoid[AquaContext].combine))

    new AquaParser[F, E, I, S](sources, liftI)
      .resolve[ValidatedCtx](mod =>
        context =>
          context.andThen(ctx =>
            prepareImports(
              // TODO factor out
              mod.imports.view
                .mapValues(ctx(_))
                .collect { case (fn, Some(fc)) => fn -> fc }
                .toMap,
              mod.body.head
            ).andThen(initCtx =>
              Semantics
                .process(
                  mod.body,
                  initCtx
                )
                .leftMap(_.map[Err](CompileError(_)))
                .map(rc => NonEmptyMap.one(mod.id, rc))
            )
          )
      )
      .map(
        _.andThen(modules =>
          Linker
            .link[I, AquaError[I, E, S], ValidatedCtx](
              modules,
              cycle => CycleError[I, E, S](cycle.map(_.id)),
              i => validNec(NonEmptyMap.one(i, Monoid.empty[AquaContext]))
            )
            .andThen { filesWithContext =>
              filesWithContext
                .foldLeft[ValidatedNec[Err, Chain[AquaProcessed[I]]]](
                  validNec(Chain.nil)
                ) {
                  case (acc, (i, Valid(context))) =>
                    acc combine validNec(
                      Chain.fromSeq(context.toNel.toList.map { case (i, c) => AquaProcessed(i, c) })
                    )
                  case (acc, (_, Invalid(errs))) =>
                    acc combine Invalid(errs)
                }
                .map(
                  _.map { ap =>
                    val compiled = backend.generate(AquaRes.fromContext(ap.context, config))
                    AquaCompiled(ap.id, compiled)
                  }
                )
            }
        )
      )
  }

  def compileTo[F[_]: Monad, E, I: Order, S[_]: Comonad, T](
    sources: AquaSources[F, E, I],
    liftI: (I, String) => LiftParser[S],
    backend: Backend,
    config: TransformConfig,
    write: AquaCompiled[I] => F[Seq[Validated[E, T]]]
  ): F[ValidatedNec[AquaError[I, E, S], Chain[T]]] =
    compile[F, E, I, S](sources, liftI, backend, config).flatMap {
      case Valid(compiled) =>
        compiled.map { ac =>
          write(ac).map(
            _.map(
              _.bimap[NonEmptyChain[AquaError[I, E, S]], Chain[T]](
                e => NonEmptyChain.one(OutputError(ac, e)),
                Chain.one
              )
            )
          )
        }.toList
          .traverse(identity)
          .map(
            _.flatten
              .foldLeft[ValidatedNec[AquaError[I, E, S], Chain[T]]](validNec(Chain.nil))(
                _ combine _
              )
          )

      case Validated.Invalid(errs) =>
        Validated.invalid[NonEmptyChain[AquaError[I, E, S]], Chain[T]](errs).pure[F]
    }
}
