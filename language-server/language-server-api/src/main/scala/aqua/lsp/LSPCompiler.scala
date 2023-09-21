package aqua.lsp

import aqua.compiler.{AquaCompiler, AquaCompilerConf, AquaError, AquaSources}
import aqua.parser.{Ast, ParserError}
import aqua.raw.RawContext
import aqua.semantics.header.{HeaderHandler, HeaderSem}

import cats.data.Validated.validNec
import cats.syntax.semigroup.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.traverse.*
import cats.syntax.either.*
import cats.{Comonad, Monad, Monoid, Order}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}

object LSPCompiler {

  private def getLspAquaCompiler[F[_]: Monad, E, I: Order, S[_]: Comonad](
    config: AquaCompilerConf
  ): AquaCompiler[F, E, I, S, LspContext[S]] = {
    implicit val rc: Monoid[LspContext[S]] = LspContext
      .implicits(
        LspContext
          .blank[S]
          .copy(raw =
            RawContext.blank.copy(parts =
              Chain.fromSeq(config.constantsList).map(const => RawContext.blank -> const)
            )
          )
      )
      .lspContextMonoid

    implicit val headerSemMonoid: Monoid[HeaderSem[S, LspContext[S]]] =
      new Monoid[HeaderSem[S, LspContext[S]]] {
        override def empty: HeaderSem[S, LspContext[S]] = HeaderSem(rc.empty, (c, _) => validNec(c))

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

    new AquaCompiler[F, E, I, S, LspContext[S]](new HeaderHandler[S, LspContext[S]](), semantics)
  }

  def compileToLsp[F[_]: Monad, E, I: Order, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    config: AquaCompilerConf
  ): F[ValidatedNec[AquaError[I, E, S], Map[I, LspContext[S]]]] = {

    val compiler = getLspAquaCompiler[F, E, I, S](config)
    compiler
      .compileRaw(sources, parser)
      .map(_.value.value.toValidated)
  }
}
