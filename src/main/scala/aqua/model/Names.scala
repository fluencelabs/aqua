package aqua.model

import aqua.parser._
import aqua.parser.lexer._
import cats.Comonad
import cats.data.Validated.Valid
import cats.data.{NonEmptyList, Validated, ValidatedNel}

// Fully resolved Scope must have no expected abilities (all resolved)
case class Names[F[_]](
  // Take vars, set vars
  // Data type is not yet known
  data: InOutAcc.Data[F] = InOutAcc.empty[F, Value[F], Var[F]],
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

  def foldVerify[F[_]: Comonad](input: List[Names[F]]): ValidatedNel[F[String], Names[F]] =
    input.foldLeft[ValidatedNel[F[String], Names[F]]](Valid(Names[F]())) {
      case (accE, ns) =>
        accE
          .andThen(acc =>
            Validated.fromEither[NonEmptyList[F[String]], Names[F]](
              NonEmptyList
                .fromList(
                  acc.abilities.validateDuplicates((v, _) => s"Duplicate ability definition `$v`", ns.abilities) :::
                    acc.types.validateDuplicates((v, _) => s"Duplicate type definition `$v`", ns.types) :::
                    acc.arrows.validateDuplicates((v, _) => s"Duplicate func definition `$v`", ns.arrows)
                )
                .toLeft(acc)
            )
          )
          .map(combine(_, ns))
          .andThen { acc =>
            Validated.fromEither[NonEmptyList[F[String]], Names[F]](
              NonEmptyList
                .fromList(
                  acc.data.validateUnresolved((v, _) => s"Unknown variable `${v}`") :::
                    acc.arrows.validateUnresolved((a, _) => s"Unknown arrow `${a}`") :::
                    acc.abilitiesResolve.validateUnresolved((a, _) => s"Unresolved ability `${a}`") :::
                    acc.abilities.validateUnresolved((a, _) => s"Undefined ability `${a}`") :::
                    acc.types.validateUnresolved((t, _) => s"Undefined type `$t`")
                )
                .toLeft(acc)
            )

          }
    }
}
