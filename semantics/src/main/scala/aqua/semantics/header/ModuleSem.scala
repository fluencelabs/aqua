package aqua.semantics.header

import aqua.parser.head.ModuleExpr
import aqua.semantics.header.HeaderHandler.{Res, error}
import aqua.semantics.header.Picker.*
import aqua.semantics.rules.locations.LocationsAlgebra

import cats.data.*
import cats.data.Validated.*
import cats.kernel.Semigroup
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.semigroup.*
import cats.syntax.validated.*
import cats.{Comonad, Monoid}

class ModuleSem[S[_]: Comonad, C: Picker](expr: ModuleExpr[S])(using
  locations: LocationsAlgebra[S, State[C, *]]
) {

  import expr.*

  def headerSem: Res[S, C] = {
    val declares = declareNames.fproductLeft(_.value) ::: declareCustom.fproductLeft(_.value)
    val names = declares.map { case (name, _) => name }.toSet

    lazy val sem = HeaderSem(
      // Save module header info
      Picker[C].blank.setModule(
        name.value,
        names
      ),
      ctx =>
        // When file is handled, check that all the declarations exists
        if (declareAll.nonEmpty)
          ctx.setModule(name.value, declares = ctx.allNames).validNec
        else
          // summarize contexts to allow redeclaration of imports
          declares.map { case (n, t) =>
            ctx
              .pick(n, None, ctx.module.nonEmpty)
              .toValidNec(
                error(
                  t,
                  s"`$n` is expected to be declared, but declaration is not found in the file"
                )
              )
              .void
          }.combineAll.as(ctx.addOccurences(declares))
    )

    word.value.fold(
      module = error(
        word,
        "Keyword `module` is deprecated, use `aqua` instead"
      ).invalidNec,
      aqua = sem.validNec
    )
  }
}
