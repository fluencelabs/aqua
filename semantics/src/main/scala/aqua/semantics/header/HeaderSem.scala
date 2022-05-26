package aqua.semantics.header

import cats.data.Validated.{invalidNec, validNec, Invalid, Valid}
import cats.{Comonad, Eval, Monoid}
import cats.data.{Chain, NonEmptyChain, NonEmptyMap, Validated, ValidatedNec}
import aqua.parser.Ast
import aqua.parser.head.*
import aqua.parser.lexer.{Ability, Token}
import aqua.raw.RawContext
import aqua.semantics.{HeaderError, SemanticError}
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import cats.instances.list.*
import cats.instances.option.*
import cats.free.Cofree
import cats.kernel.Semigroup
import Picker.*

type Res[S[_], C] = ValidatedNec[SemanticError[S], HeaderSem[S, C]]

trait HeaderSemAct[S[_], C] {

  def sem(imports: Map[String, C], header: Ast.Head[S]): Res[S, C]
}

class HeaderSemRawContext[S[_]: Comonad](implicit
  acm: Monoid[RawContext],
  headMonoid: Monoid[HeaderSem[S, RawContext]]
) extends HeaderSemAct[S, RawContext] {

  import Picker.*

  type ResAC[S[_]] = ValidatedNec[SemanticError[S], RawContext]
  type ResT[S[_], T] = ValidatedNec[SemanticError[S], T]

  type RawHeaderSem = HeaderSem[S, RawContext]

  // Helper: monoidal combine of all the childrens after parent res
  private def combineAnd(children: Chain[Res[S, RawContext]])(
    parent: Res[S, RawContext]
  ): Eval[Res[S, RawContext]] =
    Eval.later(parent |+| children.combineAll)

  // Error generator with token pointer
  private def error[T](token: Token[S], msg: String): ValidatedNec[SemanticError[S], T] =
    invalidNec(HeaderError(token, msg))

  override def sem(imports: Map[String, RawContext], header: Ast.Head[S]): Res[S, RawContext] = {
    // Resolve a filename from given imports or fail
    def resolve(f: FilenameExpr[S]): ResAC[S] =
      imports
        .get(f.fileValue)
        .map(_.pickDeclared)
        .fold[ResAC[S]](
          error(f.token, "Cannot resolve the import")
        )(validNec)

    // Get part of the declared context (for import/use ... from ... expressions)
    def getFrom(f: FromExpr[S], ctx: RawContext): ResAC[S] =
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
    def toModule(ctx: RawContext, tkn: Token[S], rename: Option[Ability[S]]): ResAC[S] =
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
    val onExpr: PartialFunction[HeaderExpr[S], Res[S, RawContext]] = {
      // Module header, like `module A declares *`
      case ModuleExpr(name, declareAll, declareNames, declareCustom) =>
        val shouldDeclare = declareNames.map(_.value).toSet ++ declareCustom.map(_.value)
        validNec(
          HeaderSem[S, RawContext](
            // Save module header info
            acm.empty.copy(
              module = Some(name.value),
              declares = shouldDeclare
            ),
            (ctx, _) =>
              // When file is handled, check that all the declarations exists
              if (declareAll.nonEmpty) {
                val all =
                  ctx.`type`("").map(_.fields.toNel.map(_._1).toList.toSet).getOrElse(Set.empty)
                validNec(
                  ctx.copy(module = Some(name.value), declares = all)
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
                    ctx.copy(module = Some(name.value), declares = shouldDeclare)
                  )
          )
        )

      case f @ ImportExpr(_) =>
        // Import everything from a file
        resolve(f).map(fc => HeaderSem[S, RawContext](fc, (c, _) => validNec(c)))
      case f @ ImportFromExpr(_, _) =>
        // Import, map declarations
        resolve(f)
          .andThen(getFrom(f, _))
          .map { ctx =>
            HeaderSem[S, RawContext](ctx, (c, _) => validNec(c))
          }

      case f @ UseExpr(_, asModule) =>
        // Import, move into a module scope
        resolve(f)
          .andThen(toModule(_, f.token, asModule))
          .map { fc =>
            HeaderSem[S, RawContext](fc, (c, _) => validNec(c))
          }

      case f @ UseFromExpr(_, _, asModule) =>
        // Import, cherry-pick declarations, move to a module scope
        resolve(f)
          .andThen(getFrom(f, _))
          .andThen(toModule(_, f.token, Some(asModule)))
          .map { fc =>
            HeaderSem[S, RawContext](fc, (c, _) => validNec(c))
          }

      case ExportExpr(pubs) =>
        // Save exports, finally handle them
        validNec(
          HeaderSem[S, RawContext](
            // Nothing there
            acm.empty,
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
                        s"File has no $name declaration or import, cannot export, available funcs: ${(initCtx |+| ctx).funcs.keys
                          .mkString(", ")}"
                      )
                    )
                }
                .foldLeft[ResT[S, Map[String, Option[String]]]](
                  validNec(ctx.exports.getOrElse(Map.empty))
                )(_ |+| _)
                .map(expCtx => ctx.copy(exports = Some(expCtx)))
          )
        )

      case HeadExpr(token) =>
        // Old file exports everything that it declares
        validNec(
          HeaderSem[S, RawContext](
            acm.empty,
            (ctx, _) => validNec(ctx.copy(exports = Some(Map.empty)))
          )
        )

      case f: FilenameExpr[S] =>
        resolve(f).map(fc => HeaderSem[S, RawContext](fc, (c, _) => validNec(c)))
    }

    Cofree
      .cata[Chain, HeaderExpr[S], Res[S, RawContext]](header) { case (expr, children) =>
        onExpr.lift.apply(expr).fold(Eval.later(children.combineAll))(combineAnd(children)(_))
      }
      .value
  }
}

case class HeaderSem[S[_], C](
  initCtx: RawContext,
  finInitCtx: (RawContext, RawContext) => ValidatedNec[SemanticError[S], C]
) {

  def finCtx: RawContext => ValidatedNec[SemanticError[S], C] =
    finInitCtx(_, initCtx)
}

object HeaderSem {}
