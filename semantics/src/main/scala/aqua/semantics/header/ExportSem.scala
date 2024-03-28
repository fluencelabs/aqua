package aqua.semantics.header

import aqua.parser.head.*
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
  acm: Monoid[C],
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

  private def finSem(ctx: C, initCtx: C): ValidatedNec[SemanticError[S], C] = {
    val pubs = expr.pubs
      .map(
        _.bimap(
          _.bimap(n => (n, n.value), n => (n, n.map(_.value))),
          _.bimap(n => (n, n.value), n => (n, n.map(_.value)))
        ).merge
      )

    val tokens = pubs.toList.flatMap {
      case ((token, name), (renameToken, _)) =>
        renameToken.map(name -> _).toList :+ (name, token)
    }

    val ctxWithExportLocations = ctx.addOccurences(tokens)
    val sumCtx = initCtx |+| ctxWithExportLocations

    pubs.map { case ((token, name), (_, rename)) =>
      println("export: pick: " + name)
      println("export: from: " + ctx.module)
      println("export: from init: " + initCtx.module)
      println("export: that declares: " + sumCtx.declares)
      sumCtx
        .pick(name, rename, declared = false)
        .as(Map(name -> rename))
        .toValid(
          error(
            token,
            s"Files has no $name declaration or import, " +
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
      .prepend(validNec(sumCtx.exports))
      .combineAll
      .map(sumCtx.setExports)
  }
}
