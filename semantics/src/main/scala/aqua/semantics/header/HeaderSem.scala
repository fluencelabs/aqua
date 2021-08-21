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
import cats.instances.list.*
import cats.instances.option.*
import cats.free.Cofree

case class HeaderSem[S[_]](
  initCtx: AquaContext,
  finCtx: AquaContext => ValidatedNec[SemanticError[S], AquaContext]
)

object HeaderSem {
  type Res[S[_]] = ValidatedNec[SemanticError[S], HeaderSem[S]]
  type ResAC[S[_]] = ValidatedNec[SemanticError[S], AquaContext]

  private implicit def headerSemMonoid[S[_]](implicit
    acm: Monoid[AquaContext]
  ): Monoid[HeaderSem[S]] =
    new Monoid[HeaderSem[S]] {
      override def empty: HeaderSem[S] = HeaderSem(acm.empty, validNec(_))

      override def combine(a: HeaderSem[S], b: HeaderSem[S]): HeaderSem[S] =
        HeaderSem(
          a.initCtx |+| b.initCtx,
          a.finCtx.andThen(_.andThen(b.finCtx))
        )
    }

  // Helper: monoidal combine of all the childrens after parent res
  private def combineAnd[S[_]](children: Chain[Res[S]])(parent: Res[S])(implicit
    acm: Monoid[AquaContext]
  ): Eval[Res[S]] =
    Eval.later(parent |+| children.combineAll)

  // Error generator with token pointer
  private def error[S[_], T](token: Token[S], msg: String): ValidatedNec[SemanticError[S], T] =
    invalidNec(HeaderError(token, msg))

  def sem[S[_]: Comonad](imports: Map[String, AquaContext], header: Ast.Head[S])(implicit
    acm: Monoid[AquaContext]
  ): Res[S] = {
    // Resolve a filename from given imports or fail
    def resolve(f: FilenameExpr[S]): ResAC[S] =
      imports
        .get(f.fileValue)
        .fold[ResAC[S]](
          error(f.token, "Cannot resolve the import")
        )(validNec)

    // Get part of the declared context (for import/use ... from ... expressions)
    def getFrom(f: FromExpr[S], ctx: AquaContext): ResAC[S] =
      f.imports
        .map[ResAC[S]](
          _.fold[ResAC[S]](
            { case (n, rn) =>
              ctx
                .pick(n.value, rn.map(_.value))
                .map(validNec)
                .getOrElse(
                  error(
                    n,
                    s"Imported file declares [${ctx.declares.mkString(", ")}], no ${n.value} declared. Try adding `declares *` to that file."
                  )
                )
            },
            { case (n, rn) =>
              ctx
                .pick(n.value, rn.map(_.value))
                .map(validNec)
                .getOrElse(
                  error(
                    n,
                    s"Imported file declares [${ctx.declares.mkString(", ")}], no ${n.value} declared. Try adding `declares *` to that file."
                  )
                )
            }
          )
        )
        .reduce

    // Convert an imported context into a module (ability)
    def toModule(ctx: AquaContext, tkn: Token[S], rename: Option[Ability[S]]): ResAC[S] =
      rename
        .map(_.value)
        .orElse(ctx.module)
        .fold[ResAC[S]](
          error(
            tkn,
            s"Used module has no `module` header. Please add `module` header or use ... as ModuleName, or switch to import"
          )
        )(modName => validNec(acm.empty.copy(abilities = Map(modName -> ctx))))

    // Handler for every header expression, will be combined later
    val onExpr: PartialFunction[HeaderExpr[S], Res[S]] = {
      // Module header, like `module A declares *`
      case ModuleExpr(name, declareAll, declareNames, declareCustom) =>
        val shouldDeclare = declareNames.map(_.value).toSet ++ declareCustom.map(_.value)
        validNec(
          HeaderSem[S](
            // Save module header info
            acm.empty.copy(
              module = Some(name.value),
              declares = shouldDeclare
            ),
            ctx =>
              // When file is handled, check that all the declarations exists
              if (declareAll.nonEmpty)
                validNec(
                  ctx.copy(declares =
                    ctx.`type`("").map(_.fields.toNel.map(_._1).toList.toSet).getOrElse(Set.empty)
                  )
                )
              else
                (
                  declareNames.map(n => n.value -> n) ::: declareCustom.map(a => a.value -> a)
                ).map[ValidatedNec[SemanticError[S], Int]] { case (n, t) =>
                  ctx
                    .pick(n, None)
                    // We just validate, nothing more
                    .map(_ => validNec(1))
                    .getOrElse(
                      error(
                        t,
                        s"`${n}` is expected to be declared, but declaration is not found in the file"
                      )
                    )
                }.combineAll
                  .map(_ =>
                    // TODO: why module name and declares is lost? where is it lost?
                    ctx.copy(module = Some(name.value), declares = shouldDeclare)
                  )
          )
        )

      case f @ ImportExpr(_) =>
        // Import everything from a file
        resolve(f).map(fc => HeaderSem[S](fc, validNec(_)))
      case f @ ImportFromExpr(_, _) =>
        // Import, map declarations
        resolve(f)
          .andThen(getFrom(f, _))
          .map(ctx => HeaderSem[S](ctx, validNec(_)))

      case f @ UseExpr(_, asModule) =>
        // Import, move into a module scope
        resolve(f)
          .andThen(toModule(_, f.token, asModule))
          .map(fc => HeaderSem[S](fc, validNec(_)))

      case f @ UseFromExpr(_, _, asModule) =>
        // Import, cherry-pick declarations, move to a module scope
        resolve(f)
          .andThen(getFrom(f, _))
          .andThen(toModule(_, f.token, Some(asModule)))
          .map(fc => HeaderSem[S](fc, validNec(_)))

      case ExportExpr(pubs) =>
        // Save exports, finally handle them
        validNec(
          HeaderSem[S](
            // Nothing there
            acm.empty,
            ctx =>
              pubs
                .map(
                  _.fold(
                    { case (n, rn) =>
                      ctx
                        .pick(n.value, rn.map(_.value), declared = false)
                        .map(validNec)
                        .getOrElse(
                          error(n, s"File has no ${n.value} declaration or import, cannot export")
                        )
                    },
                    { case (n, rn) =>
                      ctx
                        .pick(n.value, rn.map(_.value), declared = false)
                        .map(validNec)
                        .getOrElse(
                          error(n, s"File has no ${n.value} declaration or import, cannot export")
                        )
                    }
                  )
                )
                .foldLeft[ResAC[S]](validNec(ctx.exports.getOrElse(acm.empty)))(_ |+| _)
                .map(expCtx => ctx.copy(exports = Some(expCtx)))
          )
        )

      case HeadExpr(token) =>
        // Old file exports everything
        validNec(HeaderSem[S](acm.empty, ctx => validNec(ctx.copy(exports = Some(ctx)))))

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
