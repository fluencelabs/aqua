package aqua.semantics.header

import aqua.parser.Ast
import aqua.parser.head.*
import aqua.parser.lexer.{Ability, Name, Token}
import aqua.semantics.header.Picker.*
import aqua.semantics.{HeaderError, SemanticError}

import cats.data.*
import cats.data.Validated.*
import cats.free.Cofree
import cats.instances.list.*
import cats.instances.option.*
import cats.kernel.Semigroup
import cats.syntax.apply.*
import cats.syntax.bifunctor.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.semigroup.*
import cats.syntax.traverse.*
import cats.syntax.validated.*
import cats.{Comonad, Eval, Monoid}

class HeaderHandler[S[_]: Comonad, C](using
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
  private def error[T](
    token: Token[S],
    msg: String
  ): SemanticError[S] = HeaderError(token, msg)

  private def exportFuncChecks(ctx: C, token: Token[S], name: String): ResT[S, Unit] =
    Validated.condNec(
      !ctx.funcReturnAbilityOrArrow(name),
      (),
      error(
        token,
        s"The function '$name' cannot be exported, because it returns an arrow or an ability"
      )
    ) combine Validated.condNec(
      !ctx.funcAcceptAbility(name),
      (),
      error(
        token,
        s"The function '$name' cannot be exported, because it accepts an ability"
      )
    )

  def sem(imports: Map[String, C], header: Ast.Head[S]): Res[S, C] = {
    // Resolve a filename from given imports or fail
    def resolve(f: FilenameExpr[S]): ResAC[S] =
      imports
        .get(f.fileValue)
        .map(_.pickDeclared)
        .toValidNec(
          error(f.token, "Cannot resolve the import")
        )

    // Get part of the declared context (for import/use ... from ... expressions)
    def getFrom(f: FromExpr[S], ctx: C): ResAC[S] =
      ctx.pickHeader.validNec |+| f.imports
        .map(
          _.bimap(
            _.bimap(n => (n, n.value), _.map(_.value)),
            _.bimap(n => (n, n.value), _.map(_.value))
          ).merge match {
            case ((token, name), rename) =>
              ctx
                .pick(name, rename, ctx.module.nonEmpty)
                .toValidNec(
                  error(
                    token,
                    s"Imported file `declares ${ctx.declares.mkString(", ")}`, no $name declared. Try adding `declares $name` to that file."
                  )
                )
          }
        )
        .combineAll

    // Convert an imported context into a module (ability)
    def toModule(ctx: C, tkn: Token[S], rename: Option[Ability[S]]): ResAC[S] =
      rename
        .map(_.value)
        .orElse(ctx.module)
        .map(modName => picker.blank.setAbility(modName, ctx))
        .toValidNec(
          error(
            tkn,
            s"Used module has no `module` header. Please add `module` header or use ... as ModuleName, or switch to import"
          )
        )

    val handleModule: ModuleExpr[S] => HeaderSem[S, C] = {
      case ModuleExpr(name, declareAll, declareNames, declareCustom) =>
        val shouldDeclare = declareNames.map(_.value).toSet ++ declareCustom.map(_.value)

        HeaderSem(
          // Save module header info
          acm.empty.setModule(
            name.value,
            shouldDeclare
          ),
          (ctx, _) =>
            // When file is handled, check that all the declarations exists
            if (declareAll.nonEmpty)
              ctx.setModule(name.value, declares = ctx.all).validNec
            else
              (
                declareNames.fproductLeft(_.value) ::: declareCustom.fproductLeft(_.value)
              ).map { case (n, t) =>
                ctx
                  .pick(n, None, ctx.module.nonEmpty)
                  .toValidNec(
                    error(
                      t,
                      s"`$n` is expected to be declared, but declaration is not found in the file"
                    )
                  )
                  .void
              }.combineAll
                .as(
                  // TODO: why module name and declares is lost? where is it lost?
                  ctx.setModule(name.value, declares = shouldDeclare)
                )
        )
    }

    // Handler for every header expression, will be combined later
    val onExpr: HeaderExpr[S] => Res[S, C] = {
      case f @ ImportExpr(_) =>
        // Import everything from a file
        resolve(f).map(fc => HeaderSem[S, C](fc, (c, _) => validNec(c)))

      case f @ ImportFromExpr(_, _) =>
        // Import, map declarations
        resolve(f)
          .andThen(getFrom(f, _))
          .map { ctx =>
            HeaderSem(ctx, (c, _) => validNec(c))
          }

      case f @ UseExpr(_, asModule) =>
        // Import, move into a module scope
        resolve(f)
          .andThen(toModule(_, f.token, asModule))
          .map { fc =>
            HeaderSem(fc, (c, _) => validNec(c))
          }

      case f @ UseFromExpr(_, _, asModule) =>
        // Import, cherry-pick declarations, move to a module scope
        resolve(f)
          .andThen(getFrom(f, _))
          .andThen(toModule(_, f.token, Some(asModule)))
          .map { fc =>
            HeaderSem(fc, (c, _) => validNec(c))
          }

      case ExportExpr(pubs) =>
        // Save exports, finally handle them
        HeaderSem(
          // Nothing there
          picker.blank,
          (ctx, initCtx) =>
            val sumCtx = initCtx |+| ctx

            pubs
              .map(
                _.bimap(
                  _.bimap(n => (n, n.value), _.map(_.value)),
                  _.bimap(n => (n, n.value), _.map(_.value))
                ).merge
              )
              .map { case ((token, name), rename) =>
                sumCtx
                  .pick(name, rename, declared = false)
                  .as(Map(name -> rename))
                  .toValid(
                    error(
                      token,
                      s"File has no $name declaration or import, " +
                        s"cannot export, available functions: ${sumCtx.funcNames.mkString(", ")}"
                    )
                  )
                  .ensure(
                    error(
                      token,
                      s"Can not export '$name' as it is an ability"
                    )
                  )(_ => !sumCtx.isAbility(name))
                  .toValidatedNec <* exportFuncChecks(sumCtx, token, name)
              }
              .prepend(validNec(ctx.exports))
              .combineAll
              .map(ctx.setExports)
        ).validNec

      case f: FilenameExpr[S] =>
        resolve(f).map(fc => HeaderSem(fc, (c, _) => validNec(c)))
    }

    header.uncons.collect { case (m: ModuleExpr[S], other) =>
      other.map(onExpr).combineAll.map(handleModule(m) |+| _)
    }
  }
}
