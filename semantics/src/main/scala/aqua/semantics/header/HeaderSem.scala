package aqua.semantics.header

import aqua.model.AquaContext
import cats.data.Validated.{invalidNec, validNec, Invalid, Valid}
import cats.{Comonad, Eval, Monoid}
import cats.data.{Chain, NonEmptyChain, NonEmptyMap, Validated, ValidatedNec}
import aqua.parser.Ast
import aqua.parser.head.*
import aqua.parser.lexer.{Ability, Token}
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
  type ResAC[S[_]] = ValidatedNec[SemanticError[S], AquaContext]

  implicit def headerSemMonoid[S[_]](implicit acm: Monoid[AquaContext]): Monoid[HeaderSem[S]] =
    new Monoid[HeaderSem[S]] {
      override def empty: HeaderSem[S] = HeaderSem(acm.empty, validNec(_))

      override def combine(a: HeaderSem[S], b: HeaderSem[S]): HeaderSem[S] =
        HeaderSem(
          a.initCtx |+| b.initCtx,
          a.finCtx.andThen(_.andThen(b.finCtx))
        )
    }

  private def combineAnd[S[_]](children: Chain[Res[S]])(parent: Res[S])(implicit
    acm: Monoid[AquaContext]
  ): Eval[Res[S]] =
    Eval.later(parent |+| children.combineAll)

  private def error[S[_], T](token: Token[S], msg: String): ValidatedNec[SemanticError[S], T] =
    invalidNec(HeaderError(token, msg))

  def sem[S[_]: Comonad](imports: Map[String, AquaContext], header: Ast.Head[S])(implicit
    acm: Monoid[AquaContext]
  ): Res[S] = {
    def resolve(f: FilenameExpr[S]): ResAC[S] =
      imports
        .get(f.fileValue)
        .fold(
          error(f.token, "Cannot resolve the import")
        )(validNec)

    def getFrom(f: FromExpr[S], ctx: AquaContext): ResAC[S] =
      f.imports
        .map[ResAC[S]](
          _.fold(
            { case (n, rn) =>
              val targetName = rn.map(_.value).getOrElse(n.value)

              ctx.funcs
                .get(n.value)
                .map(fnc => acm.empty.copy(funcs = Map(targetName -> fnc)))
                .orElse(
                  ctx.values.get(n.value).map(vc => acm.empty.copy(values = Map(targetName -> vc)))
                )
                .map(validNec)
                .getOrElse(error(n, s"Imported file has no ${n.value} declaration"))
            },
            { case (a, ra) =>
              val targetName = ra.map(_.value).getOrElse(a.value)

              ctx.services
                .get(a.value)
                .map(sc => acm.empty.copy(services = Map(targetName -> sc)))
                .orElse(
                  ctx.types.get(a.value).map(tc => acm.empty.copy(types = Map(targetName -> tc)))
                )
                .orElse(
                  ctx.abilities
                    .get(a.value)
                    .map(ac => acm.empty.copy(abilities = Map(targetName -> ac)))
                )
                .map(validNec)
                .getOrElse(error(a, s"Imported file has no ${a.value} declaration"))
            }
          )
        )
        .reduce

    def toModule(ctx: AquaContext, tkn: Token[S], rename: Option[Ability[S]]): ResAC[S] =
      rename
        .map(_.value)
        .orElse(ctx.module)
        .fold[ResAC[S]](
          error(
            tkn,
            "Used module has no `module` header. Please add `module` header or use ... as ModuleName, or switch to import"
          )
        )(modName => validNec(acm.empty.copy(abilities = Map(modName -> ctx))))

    val onExpr: PartialFunction[HeaderExpr[S], Res[S]] = {
      case ModuleExpr(name, exportAll, declareNames, declareCustom) =>
        validNec(HeaderSem[S](acm.empty.copy(module = Some(name.value)), validNec(_)))

      case f @ ImportExpr(_) =>
        resolve(f).map(fc => HeaderSem[S](fc, validNec(_)))
      case f @ ImportFromExpr(_, _) =>
        resolve(f)
          .andThen(getFrom(f, _))
          .map(ctx => HeaderSem[S](ctx, validNec(_)))

      case f @ UseExpr(_, asModule) =>
        resolve(f)
          .andThen(toModule(_, f.token, asModule))
          .map(fc => HeaderSem[S](fc, validNec(_)))

      case f @ UseFromExpr(_, _, asModule) =>
        resolve(f)
          .andThen(getFrom(f, _))
          .andThen(toModule(_, f.token, Some(asModule)))
          .map(fc => HeaderSem[S](fc, validNec(_)))

      case f: FilenameExpr[S] =>
        resolve(f).map(fc => HeaderSem[S](fc, validNec(_)))
    }

    Cofree
      .cata[Chain, HeaderExpr[S], Res[S]](header) { case (expr, children) =>
        onExpr.lift.apply(expr).fold(Eval.later(children.combineAll))(combineAnd(children)(_))
      }
      .value
  }

}
