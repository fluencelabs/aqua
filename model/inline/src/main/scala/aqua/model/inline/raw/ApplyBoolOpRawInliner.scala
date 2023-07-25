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
import aqua.raw.value.ApplyBoolOpRaw.BoolOpRaw.*

import cats.data.Chain
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.applicative.*

object ApplyBoolOpRawInliner extends RawInliner[ApplyBoolOpRaw] {

  override def apply[S: Mangler: Exports: Arrows](
    raw: ApplyBoolOpRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = for {
    left <- unfold(raw.left)
    (lmodel, linline) = left
    right <- unfold(raw.right)
    (rmodel, rinline) = right

    result <- (lmodel, rmodel) match {
      // Optimize in case of left value is known at compile time
      case (LiteralModel.Bool(lvalue), _) =>
        (raw.op match {
          case And if !lvalue => (LiteralModel.bool(false), linline)
          case Or if lvalue => (LiteralModel.bool(true), linline)
          case _ => (rmodel, Inline(linline.predo ++ rinline.predo))
        }).pure[State[S, *]]
      // Optimize in case of right value is known at compile time and it has no computation
      case (_, LiteralModel.Bool(rvalue)) if rinline.predo.isEmpty =>
        (raw.op match {
          case And if !rvalue => (LiteralModel.bool(false), linline)
          case Or if rvalue => (LiteralModel.bool(true), linline)
          case _ => (lmodel, linline)
        }).pure[State[S, *]]
      // Produce unoptimized inline
      case _ => fullInline(lmodel, rmodel, linline, rinline, raw.op)
    }
  } yield result

  private def fullInline[S: Mangler: Exports: Arrows](
    lmodel: ValueModel,
    rmodel: ValueModel,
    linline: Inline,
    rinline: Inline,
    op: ApplyBoolOpRaw.BoolOpRaw
  ): State[S, (ValueModel, Inline)] = {
    val (name, compareWith) = op match {
      case And => ("and", true)
      case Or => ("or", false)
    }

    /*
     * (seq
     *   <left-inline>
     *   (xor
     *     (match <left-res> <compare-with>
     *       (seq
     *         <right-inline>
     *         (ap <right-res> <res-name>)
     *       )
     *     )
     *     (ap <left-res> <res-name>)
     *   )
     * )
     *
     * TODO: Handle errors in <right-inline>
     */
    val predo = (resName: String) =>
      SeqModel.wrap(
        linline.predo :+ XorModel.wrap(
          MatchMismatchModel(
            lmodel,
            LiteralModel.bool(compareWith),
            shouldMatch = true
          ).wrap(
            SeqModel.wrap(
              rinline.predo :+ FlattenModel(
                rmodel,
                resName
              ).leaf
            )
          ),
          FlattenModel(
            lmodel,
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
