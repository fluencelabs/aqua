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
import cats.syntax.validated.*
import cats.{Comonad, Monoid}

class ModuleSem[S[_]: Comonad, C: Picker](expr: ModuleExpr[S])(using
  acm: Monoid[C],
  locations: LocationsAlgebra[S, State[C, *]]
) {

  import expr.*

  def headerSem: Res[S, C] = {
    val shouldDeclare = declareNames.map(_.value).toSet ++ declareCustom.map(_.value)

    lazy val sem = HeaderSem(
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
          }.combineAll.as {
            val tokens = declareNames.map(n => n.value -> n) ++ declareCustom.map(a => a.value -> a)
            val ctxWithDeclaresLoc = ctx.addOccurences(tokens)
            println("after combineAll: " + ctx.all)
            // TODO: why module name and declares is lost? where is it lost?
            ctxWithDeclaresLoc.setModule(name.value, declares = shouldDeclare)
          }
            
          
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
