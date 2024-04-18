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

  def headerSem: Res[S, C] = {
    lazy val sem = HeaderSem(
      // Save module header info
      Picker[C].blank.setModule(expr.name.value),
      ctx =>
        expr.declares match {
          case None => ctx.validNec
          case Some(ModuleExpr.Declares.All(_)) =>
            ctx.setDeclares(ctx.allNames).validNec
          case Some(ModuleExpr.Declares.Names(declareNames)) =>
            val declares = declareNames.fproductLeft(_.value).toList
            val names = declares.map { case (name, _) => name }.toSet
            val res = ctx.setDeclares(names).addOccurences(declares)

            // summarize contexts to allow redeclaration of imports
            declares.map { case (n, t) =>
              res
                .pick(n, None, ctx.module.nonEmpty)
                .toValidNec(
                  error(
                    t,
                    s"`$n` is expected to be declared, but declaration is not found in the file"
                  )
                )
                .void
            // TODO: Should not it be possible to make `.combineAll` the final result?
            // Seems like `.pick` does not return much information
            }.combineAll.as(res)
        }
    )

    expr.word.value.fold(
      module = error(
        expr.word,
        "Keyword `module` is deprecated, use `aqua` instead"
      ).invalidNec,
      aqua = sem.validNec
    )
  }
}
