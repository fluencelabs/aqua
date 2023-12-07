package aqua.lsp

import aqua.compiler.{AquaCompiler, AquaCompilerConf, AquaError, AquaSources}
import aqua.parser.{Ast, ParserError}
import aqua.raw.RawContext
import aqua.semantics.header.{HeaderHandler, HeaderSem}

import cats.data.Validated.validNec
import cats.data.{Chain, Validated, ValidatedNec}
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.semigroup.*
import cats.{Comonad, Monad, Monoid, Order}

object LSPCompiler {

  private def getLspAquaCompiler[F[_]: Monad, E, I: Order, S[_]: Comonad](
    config: AquaCompilerConf
  ): AquaCompiler[F, E, I, S, LspContext[S]] = {
    given Monoid[LspContext[S]] = LspContext
      .implicits(
        LspContext.blank.copy(raw =
          RawContext.blank.copy(
            parts = Chain
              .fromSeq(config.constantsList)
              .map(const => RawContext.blank -> const)
          )
        )
      )
      .lspContextMonoid

    given Monoid[HeaderSem[S, LspContext[S]]] with {
      override def empty: HeaderSem[S, LspContext[S]] =
        HeaderSem(Monoid[LspContext[S]].empty, (c, _) => validNec(c))

      override def combine(
        a: HeaderSem[S, LspContext[S]],
        b: HeaderSem[S, LspContext[S]]
      ): HeaderSem[S, LspContext[S]] = {
        HeaderSem(
          a.initCtx |+| b.initCtx,
          (c, i) => a.finInitCtx(c, i).andThen(b.finInitCtx(_, i))
        )
      }
    }

    val semantics = new LspSemantics[S]()

    new AquaCompiler[F, E, I, S, LspContext[S]](
      new HeaderHandler(),
      semantics
    )
  }

  def compileToLsp[F[_]: Monad, E, I: Order, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    config: AquaCompilerConf
  ): F[ValidatedNec[AquaError[I, E, S], Map[I, LspContext[S]]]] = {

    val compiler = getLspAquaCompiler[F, E, I, S](config)
    compiler
      .compileRaw(sources, parser)
      // NOTE: Ignore warnings here as
      // they are collected inside context
      .map(_.value.value.toValidated)
  }
}
