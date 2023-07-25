package aqua.model.inline.raw

import aqua.model.*
import aqua.model.inline.raw.RawInliner
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{AbilityRaw, LiteralRaw, MakeStructRaw}
import cats.data.{NonEmptyList, NonEmptyMap, State}
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.{unfold, valueToModel}
import aqua.types.{ArrowType, ScalarType}
import aqua.raw.value.ApplyBoolOpRaw

import cats.data.Chain
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.apply.*
import cats.syntax.foldable.*

object ApplyBoolOpRawInliner extends RawInliner[ApplyBoolOpRaw] {

  override def apply[S: Mangler: Exports: Arrows](
    raw: ApplyBoolOpRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] =
    (unfold(raw.left), unfold(raw.right)).flatMapN { case ((lm, linline), (rm, rinline)) =>
      ???
    }
}
