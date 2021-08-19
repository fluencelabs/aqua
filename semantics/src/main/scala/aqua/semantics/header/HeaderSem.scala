package aqua.semantics.header

import aqua.model.AquaContext
import cats.data.Validated.{invalidNec, validNec, Invalid, Valid}
import cats.{Comonad, Eval, Monoid}
import cats.data.{Chain, NonEmptyChain, NonEmptyMap, Validated, ValidatedNec}
import aqua.parser.Ast
import aqua.parser.head.*
import aqua.semantics.{HeaderError, SemanticError}
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import cats.free.Cofree

case class HeaderSem[S[_]](
  initCtx: AquaContext,
  finCtx: AquaContext => ValidatedNec[SemanticError[S], AquaContext]
)

object HeaderSem {
  type Res[S[_]] = ValidatedNec[SemanticError[S], HeaderSem[S]]

  implicit def headerSemMonoid[S[_]](implicit acm: Monoid[AquaContext]): Monoid[HeaderSem[S]] =
    new Monoid[HeaderSem[S]] {
      override def empty: HeaderSem[S] = HeaderSem(acm.empty, validNec(_))

      override def combine(a: HeaderSem[S], b: HeaderSem[S]): HeaderSem[S] =
        HeaderSem(
          a.initCtx |+| b.initCtx,
          a.finCtx.andThen(_.andThen(b.finCtx))
        )
    }

  def combineAnd[S[_]](children: Chain[Res[S]])(parent: Res[S])(implicit
    acm: Monoid[AquaContext]
  ): Eval[Res[S]] =
    Eval.later(parent |+| children.combineAll)

  def sem[S[_]: Comonad](imports: Map[String, AquaContext], header: Ast.Head[S])(implicit
    acm: Monoid[AquaContext]
  ): Res[S] =
    Cofree
      .cata[Chain, HeaderExpr[S], Res[S]](header) {
        case (ModuleExpr(name, exportAll, declareNames, declareCustom), children) =>
          combineAnd(children)(
            validNec(Monoid[HeaderSem[S]].empty)
          )
        case (f: FilenameExpr[S], children) =>
          combineAnd(children)(
            imports
              .get(f.fileValue)
              .fold(
                invalidNec(HeaderError(f.token, "Cannot resolve the import"))
              )(fc => validNec(HeaderSem[S](fc, validNec(_))))
          )
        case (_, children) =>
          Eval.later(children.combineAll)
      }
      .value

}
