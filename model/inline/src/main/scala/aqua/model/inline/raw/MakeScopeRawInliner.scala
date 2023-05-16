package aqua.model.inline.raw

import aqua.model.{CallModel, CallServiceModel, LiteralModel, OpModel, SeqModel, ValueModel, VarModel}
import aqua.model.inline.raw.RawInliner
import cats.data.Chain
import aqua.model.inline.state.{Arrows, Exports, Mangler, Scopes}
import aqua.raw.value.{LiteralRaw, ScopeRaw, MakeStructRaw}
import cats.data.{NonEmptyList, NonEmptyMap, State}
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.{unfold, valueToModel}
import aqua.types.{ArrowType, ScalarType}
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.apply.*

object MakeScopeRawInliner extends RawInliner[ScopeRaw] {

  override def apply[S: Mangler: Exports: Arrows: Scopes](
    raw: ScopeRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    for {
      name <- Mangler[S].findAndForbidName(raw.scopeType.name)
      foldedFields <- raw.fieldsAndArrows.nonEmptyTraverse(unfold(_))
      varModel = VarModel(name, raw.baseType)
      valsInline = foldedFields.toSortedMap.values.map(_._2).fold(Inline.empty)(_ |+| _).desugar
      all = foldedFields.map(_._1).toNel.map {
        case (n, VarModel(name, ArrowType(_, _), _)) =>
          (None, Option(n -> name), None)
        case (n, VarModel(name, _, _)) =>
          (Option(n -> name), None, None)
        case (n, l: LiteralModel) =>
          (None, None, Option(n -> l))
      }
      vars = all.map(_._1).toList.flatten.toMap
      arrows = all.map(_._2).toList.flatten.toMap
      literals = all.map(_._3).toList.flatten.toMap
      _ <- Scopes[S].save(name, vars, arrows, literals)
    } yield {
      (
        varModel,
        Inline(
          valsInline.flattenValues,
          Chain.one(SeqModel.wrap((valsInline.predo).toList: _*))
        )
      )
    }
  }
}
