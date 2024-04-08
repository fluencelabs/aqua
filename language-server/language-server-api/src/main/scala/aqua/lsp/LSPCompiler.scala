package aqua.lsp

import aqua.compiler.{AquaCompiler, AquaCompilerConf, AquaError, AquaSources}
import aqua.parser.{Ast, ParserError}
import aqua.raw.{ConstantRaw, RawContext}
import aqua.semantics.FileId
import aqua.semantics.header.{HeaderHandler, HeaderSem}
import aqua.semantics.rules.locations.LocationsAlgebra

import cats.data.Validated.validNec
import cats.data.{Chain, State, Validated, ValidatedNec}
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.semigroup.*
import cats.{Comonad, Monad, Monoid, Order, Show}

object LSPCompiler {

  private def getLspAquaCompiler[F[_]: Monad, E, I: FileId, S[_]: Comonad](
    config: AquaCompilerConf
  ): AquaCompiler[F, E, I, S, LspContext[S]] = {
    given Monoid[LspContext[S]] = LspContext
      .implicits(
        LspContext.blank.copy(raw =
          RawContext.blank.copy(
            parts = Chain
              .fromSeq(config.constants ++ ConstantRaw.defaultConstants(config.relayVarName))
              .map(const => RawContext.blank -> const)
          )
        )
      )
      .lspContextMonoid

    given Monoid[HeaderSem[S, LspContext[S]]] with {
      override def empty: HeaderSem[S, LspContext[S]] =
        HeaderSem.fromInit(Monoid[LspContext[S]].empty)

      override def combine(
        a: HeaderSem[S, LspContext[S]],
        b: HeaderSem[S, LspContext[S]]
      ): HeaderSem[S, LspContext[S]] = {
        HeaderSem(
          a.init |+| b.init,
          (c) => a.fin(c).andThen(b.fin)
        )
      }
    }

    val semantics = new LspSemantics[S]()

    given LocationsAlgebra[S, State[LspContext[S], *]] =
      LocationsInterpreter[S, LspContext[S]]()

    new AquaCompiler[F, E, I, S, LspContext[S]](
      new HeaderHandler(),
      semantics
    )
  }

  def compileToLsp[F[_]: Monad, E, I: FileId, S[_]: Comonad](
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
