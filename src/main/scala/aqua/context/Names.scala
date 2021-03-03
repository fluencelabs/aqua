package aqua.context

import aqua.parser._
import aqua.parser.lexer._
import aqua.context.marker.{ArrowMarker, TypeMarker}
import cats.Comonad
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated, ValidatedNel}

import scala.collection.immutable.Queue

// Fully resolved Scope must have no expected abilities (all resolved)
/*case class Names[F[_]](

  // Abilities can be imported or set
  abilitiesResolve: InOutAcc.AbilitiesResolve[F] = InOutAcc.empty[F, Ability[F], AbilityResolve[F]],
  // Abilities can be defined and expected
  abilities: InOutAcc.Abilities[F] = InOutAcc.empty[F, Ability[F], DefService[F]],
  // Types can be defined and expected
  types: InOutAcc.Types[F] = InOutAcc.empty[F, CustomType[F], TypeMarker[F]],
  // We don't know the types yet
  arrows: InOutAcc.Arrows[F] = InOutAcc.empty[F, ArrowName[F], ArrowMarker[F]]
)

object Names {

  def combine[F[_]: Comonad](a: Names[F], b: Names[F]): Names[F] =
    Names[F](
      data = a.data combine b.data,
      abilitiesResolve = a.abilitiesResolve combine b.abilitiesResolve,
      abilities = a.abilities combine b.abilities,
      types = a.types combine b.types,
      arrows = a.arrows combine b.arrows
    )

  def blockNames[F[_]: Comonad](block: Block[F]): Names[F] =
    Names[F](
      data = InOutAcc.Data.block(block),
      abilitiesResolve = InOutAcc.AbilitiesResolve.block(block),
      abilities = InOutAcc.Abilities.block(block),
      types = InOutAcc.Types.block(block),
      arrows = InOutAcc.Arrows.block(block)
    )

  def foldVerify[F[_]: Comonad](input: List[Names[F]]): ValidatedNel[F[String], Names[F]] = {
    val (errs, names) = input.foldLeft[(Queue[F[String]], Names[F])]((Queue.empty, Names[F]())) {
      case ((errs, acc), ns) =>
        val combined = combine(acc, ns)
        errs
          .appendedAll(
            acc.abilities.validateDuplicates((v, _) => s"Duplicate ability definition `$v`", ns.abilities) :::
              acc.types.validateDuplicates((v, _) => s"Duplicate type definition `$v`", ns.types) :::
              acc.arrows.validateDuplicates((v, _) => s"Duplicate func definition `$v`", ns.arrows)
          )
          .appendedAll(
            combined.data.validateUnresolved((v, _) => s"Unknown variable `${v}`") :::
              combined.arrows.validateUnresolved((a, _) => s"Unknown arrow `${a}`") :::
              combined.abilitiesResolve.validateUnresolved((a, _) => s"Unresolved ability `${a}`") :::
              combined.abilities.validateUnresolved((a, _) => s"Undefined ability `${a}`") :::
              combined.types.validateUnresolved((t, _) => s"Undefined type `$t`")
          ) -> combined
    }
    NonEmptyList.fromList(errs.toList).fold[ValidatedNel[F[String], Names[F]]](Valid(names))(Invalid(_))
  }

}
 */
