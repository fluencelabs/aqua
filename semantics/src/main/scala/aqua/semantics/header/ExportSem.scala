package aqua.semantics.header

import aqua.parser.head.*
import aqua.parser.lexer.QName
import aqua.parser.lexer.Token
import aqua.semantics.SemanticError
import aqua.semantics.header.HeaderHandler.*
import aqua.semantics.header.Picker.*
import aqua.semantics.rules.locations.LocationsAlgebra

import cats.data.*
import cats.data.Validated.*
import cats.instances.option.*
import cats.kernel.Semigroup
import cats.syntax.apply.*
import cats.syntax.bifunctor.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.semigroup.*
import cats.syntax.validated.*
import cats.{Comonad, Monoid}

class ExportSem[S[_]: Comonad, C](expr: ExportExpr[S])(using
  picker: Picker[C],
  locations: LocationsAlgebra[S, State[C, *]]
) {

  private def exportFuncChecks(
    ctx: C,
    token: Token[S],
    name: String
  ): ValidatedNec[SemanticError[S], Unit] =
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

  def headerSem: Res[S, C] = {
    // Save exports, finally handle them
    HeaderSem(
      // Nothing there
      picker.blank,
      finSem
    ).validNec
  }

  private def finSem(ctx: C): ValidatedNec[SemanticError[S], C] = {
    val tokens = expr.pubs.toList.flatMap { case QName.As(name, rename) =>
      rename.map(name.value -> _).toList :+ (name.value, name)
    }

    val resCtx = ctx.addOccurences(tokens)

    expr.pubs.map { case QName.As(name, rename) =>
      resCtx
        // TODO: Refactor to PName
        .pick(name.value, rename.map(_.value), declared = false)
        .as(Map(name.value -> rename.map(_.value)))
        .toValid(
          error(
            name,
            s"Files has no '${name.value}' declaration or import, " +
              s"cannot export, available functions: ${resCtx.funcNames.mkString(", ")}"
          )
        )
        .ensure(
          error(
            name,
            s"Can not export '${name.value}' as it is an ability"
          )
        )(_ => !resCtx.isAbility(name.value))
        .toValidatedNec <* exportFuncChecks(resCtx, name, name.value)
    }
      .prepend(validNec(resCtx.exports))
      .combineAll
      .map(resCtx.setExports)
  }
}
