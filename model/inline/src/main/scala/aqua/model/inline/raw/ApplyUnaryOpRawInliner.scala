package aqua.model.inline.raw

import aqua.model.*
import aqua.model.inline.raw.RawInliner
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{AbilityRaw, LiteralRaw, MakeStructRaw}
import cats.data.{NonEmptyList, NonEmptyMap, State}
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.{unfold, valueToModel}
import aqua.types.{ArrowType, ScalarType}
import aqua.raw.value.ApplyUnaryOpRaw
import aqua.raw.value.ApplyUnaryOpRaw.Op.*

import cats.data.Chain
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.applicative.*

object ApplyUnaryOpRawInliner extends RawInliner[ApplyUnaryOpRaw] {

  override def apply[S: Mangler: Exports: Arrows](
    raw: ApplyUnaryOpRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = for {
    value <- unfold(raw.value)
    (vm, vinline) = value

    result <- vm match {
      // Optimize in case of value is known at compile time
      case LiteralModel.Bool(bvalue) =>
        (raw.op match {
          case Not => (LiteralModel.bool(!bvalue), vinline)
        }).pure[State[S, *]]
      // Produce unoptimized inline
      case _ => fullInline(vm, vinline, raw.op)
    }
  } yield result

  private def fullInline[S: Mangler: Exports: Arrows](
    vm: ValueModel,
    vinline: Inline,
    op: ApplyUnaryOpRaw.Op
  ): State[S, (ValueModel, Inline)] = {
    val name = op match {
      case Not => "not"
    }

    /*
     * (seq
     *   <value-inline>
     *   (xor
     *     (match <value-res> true
     *       (ap false <res-name>)
     *     )
     *     (ap true <res-name>)
     *   )
     * )
     */
    val predo = (resName: String) =>
      SeqModel.wrap(
        vinline.predo :+ XorModel.wrap(
          MatchMismatchModel(
            vm,
            LiteralModel.bool(true),
            shouldMatch = true
          ).wrap(
            FlattenModel(
              LiteralModel.bool(false),
              resName
            ).leaf
          ),
          FlattenModel(
            LiteralModel.bool(true),
            resName
          ).leaf
        )
      )

    Mangler[S]
      .findAndForbidName(name)
      .map(resName =>
        (
          VarModel(resName, ScalarType.bool),
          Inline(Chain.one(predo(resName)))
        )
      )
  }
}
