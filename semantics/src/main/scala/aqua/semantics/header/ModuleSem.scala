package aqua.semantics.header

import aqua.helpers.data.PName
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

class ModuleSem[S[_]: Comonad, C: Monoid: Picker](expr: ModuleExpr[S])(using
  locations: LocationsAlgebra[S, State[C, *]]
) {

  def headerSem: Res[S, C] = {
    lazy val pname = expr.name.toPName
    lazy val sem = HeaderSem.fromFin((ctx: C) => {
      (expr.declares match {
        case None => ctx.validNec
        case Some(ModuleExpr.Declares.All(_)) =>
          val names = ctx.allNames.map(PName.simpleUnsafe).toSet
          ctx.setDeclares(names).validNec
        case Some(ModuleExpr.Declares.Names(declareNames)) =>
          val declares = declareNames.fproductLeft(_.value).toList
          val names = declareNames.map(_.toPName).toList.toSet
          val res = ctx.setDeclares(names).addOccurences(declares)

          // summarize contexts to allow redeclaration of imports
          declares.map { case (n, t) =>
            res
              .pick(t.toPName, rename = None)
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
      }).map(ctx =>
        ctx
          .scoped(pname)
          .setModule(Some(pname))
          .setDeclares(ctx.declares.map(_.prepended(pname)))
      )
    })

    expr.word.value.fold(
      module = error(
        expr.word,
        "Keyword `module` is deprecated, use `aqua` instead"
      ).invalidNec,
      aqua = sem.validNec
    )
  }
}
