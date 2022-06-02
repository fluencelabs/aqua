package aqua.semantics.header

import aqua.parser.Ast
import aqua.parser.head.*
import aqua.parser.lexer.{Ability, Token}
import aqua.raw.RawContext
import aqua.semantics.header.Picker.*
import aqua.semantics.{HeaderError, SemanticError}
import cats.data.Validated.{invalidNec, validNec, Invalid, Valid}
import cats.data.*
import cats.free.Cofree
import cats.instances.list.*
import cats.instances.option.*
import cats.kernel.Semigroup
import cats.syntax.foldable.*
import cats.syntax.monoid
import cats.syntax.semigroup.*
import cats.{Comonad, Eval, Monoid}

class HeaderHandler[S[_]: Comonad, C](implicit
  acm: Monoid[C],
  headMonoid: Monoid[HeaderSem[S, C]],
  picker: Picker[C]
) {

  type Res[S[_], C] = ValidatedNec[SemanticError[S], HeaderSem[S, C]]
  type ResAC[S[_]] = ValidatedNec[SemanticError[S], C]
  type ResT[S[_], T] = ValidatedNec[SemanticError[S], T]

  // Helper: monoidal combine of all the childrens after parent res
  private def combineAnd(children: Chain[Res[S, C]])(
    parent: Res[S, C]
  ): Eval[Res[S, C]] =
    Eval.later(parent |+| children.combineAll)

  // Error generator with token pointer
  private def error[T](token: Token[S], msg: String): ValidatedNec[SemanticError[S], T] =
    invalidNec(HeaderError(token, msg))

  def sem(imports: Map[String, C], header: Ast.Head[S]): Res[S, C] = {
    // Resolve a filename from given imports or fail
    def resolve(f: FilenameExpr[S]): ResAC[S] =
      imports
        .get(f.fileValue)
        .map(_.pickDeclared)
        .fold[ResAC[S]](
          error(f.token, "Cannot resolve the import")
        )(validNec)

    // Get part of the declared context (for import/use ... from ... expressions)
    def getFrom(f: FromExpr[S], ctx: C): ResAC[S] =
      f.imports
        .map[ResAC[S]](
          _.fold[ResAC[S]](
            { case (n, rn) =>
              ctx
                .pick(n.value, rn.map(_.value), ctx.module.nonEmpty)
                .map(validNec)
                .getOrElse(
                  error(
                    n,
                    s"Imported file `declares ${ctx.declares.mkString(", ")}`, no ${n.value} declared. Try adding `declares ${n.value}` to that file."
                  )
                )
            },
            { case (n, rn) =>
              ctx
                .pick(n.value, rn.map(_.value), ctx.module.nonEmpty)
                .map(validNec)
                .getOrElse(
                  error(
                    n,
                    s"Imported file `declares ${ctx.declares.mkString(", ")}`, no ${n.value} declared. Try adding `declares ${n.value}` to that file."
                  )
                )
            }
          )
        )
        .foldLeft[ResAC[S]](validNec(ctx.pickHeader))(_ |+| _)

    // Convert an imported context into a module (ability)
    def toModule(ctx: C, tkn: Token[S], rename: Option[Ability[S]]): ResAC[S] =
      rename
        .map(_.value)
        .orElse(ctx.module)
        .fold[ResAC[S]](
          error(
            tkn,
            s"Used module has no `module` header. Please add `module` header or use ... as ModuleName, or switch to import"
          )
        )(modName => validNec(picker.blank.setAbility(modName, ctx)))

    // Handler for every header expression, will be combined later
    val onExpr: PartialFunction[HeaderExpr[S], Res[S, C]] = {
      // Module header, like `module A declares *`
      case ModuleExpr(name, declareAll, declareNames, declareCustom) =>
        val shouldDeclare = declareNames.map(_.value).toSet ++ declareCustom.map(_.value)
        validNec(
          HeaderSem[S, C](
            // Save module header info
            acm.empty.setModule(
              name.value,
              shouldDeclare
            ),
            (ctx, _) =>
              // When file is handled, check that all the declarations exists
              if (declareAll.nonEmpty) {
                validNec(
                  ctx.setModule(name.value, declares = ctx.all)
                )
              } else
                (
                  declareNames.map(n => n.value -> n) ::: declareCustom.map(a => a.value -> a)
                ).map[ValidatedNec[SemanticError[S], Int]] { case (n, t) =>
                  ctx
                    .pick(n, None, ctx.module.nonEmpty)
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
                    ctx.setModule(name.value, declares = shouldDeclare)
                  )
          )
        )

      case f @ ImportExpr(_) =>
        // Import everything from a file
        resolve(f).map(fc => HeaderSem[S, C](fc, (c, _) => validNec(c)))
      case f @ ImportFromExpr(_, _) =>
        // Import, map declarations
        resolve(f)
          .andThen(getFrom(f, _))
          .map { ctx =>
            HeaderSem[S, C](ctx, (c, _) => validNec(c))
          }

      case f @ UseExpr(_, asModule) =>
        // Import, move into a module scope
        resolve(f)
          .andThen(toModule(_, f.token, asModule))
          .map { fc =>
            HeaderSem[S, C](fc, (c, _) => validNec(c))
          }

      case f @ UseFromExpr(_, _, asModule) =>
        // Import, cherry-pick declarations, move to a module scope
        resolve(f)
          .andThen(getFrom(f, _))
          .andThen(toModule(_, f.token, Some(asModule)))
          .map { fc =>
            HeaderSem[S, C](fc, (c, _) => validNec(c))
          }

      case ExportExpr(pubs) =>
        // Save exports, finally handle them
        validNec(
          HeaderSem[S, C](
            // Nothing there
            picker.blank,
            (ctx, initCtx) =>
              pubs
                .map(
                  _.fold[(Token[S], String, Option[String])](
                    nrn => (nrn._1, nrn._1.value, nrn._2.map(_.value)),
                    nrn => (nrn._1, nrn._1.value, nrn._2.map(_.value))
                  )
                )
                .map { case (token, name, rename) =>
                  (initCtx |+| ctx)
                    .pick(name, rename, declared = false)
                    .map(_ => Map(name -> rename))
                    .map(validNec)
                    .getOrElse(
                      error(
                        token,
                        s"File has no $name declaration or import, cannot export, available funcs: ${(initCtx |+| ctx).funcNames
                          .mkString(", ")}"
                      )
                    )
                }
                .foldLeft[ResT[S, Map[String, Option[String]]]](
                  validNec(ctx.exports.getOrElse(Map.empty))
                )(_ |+| _)
                .map(expCtx => ctx.setExports(expCtx))
          )
        )

      case HeadExpr(token) =>
        // Old file exports everything that it declares
        validNec(
          HeaderSem[S, C](
            acm.empty,
            (ctx, _) => validNec(ctx.setExports(Map.empty))
          )
        )

      case f: FilenameExpr[S] =>
        resolve(f).map(fc => HeaderSem[S, C](fc, (c, _) => validNec(c)))
    }

    Cofree
      .cata[Chain, HeaderExpr[S], Res[S, C]](header) { case (expr, children) =>
        onExpr.lift.apply(expr).fold(Eval.later(children.combineAll))(combineAnd(children)(_))
      }
      .value
  }
}
