package aqua.model.inline.raw

import aqua.model.*
import aqua.model.inline.raw.RawInliner
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{AbilityRaw, LiteralRaw, MakeStructRaw}
import cats.data.{NonEmptyList, NonEmptyMap, State}
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.{unfold, valueToModel}
import aqua.types.{ArrowType, ScalarType}
import aqua.raw.value.ApplyBinaryOpRaw
import aqua.raw.value.ApplyBinaryOpRaw.Op.*
import aqua.model.inline.Inline.MergeMode

import cats.data.Chain
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.applicative.*

object ApplyBinaryOpRawInliner extends RawInliner[ApplyBinaryOpRaw] {

  private type BoolOp = And.type | Or.type
  private type EqOp = Eq.type | Neq.type

  override def apply[S: Mangler: Exports: Arrows](
    raw: ApplyBinaryOpRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = for {
    left <- unfold(raw.left)
    (lmodel, linline) = left
    right <- unfold(raw.right)
    (rmodel, rinline) = right

    result <- raw.op match {
      case op @ (And | Or) => inlineBoolOp(lmodel, rmodel, linline, rinline, op)
      case op @ (Eq | Neq) => inlineEqOp(lmodel, rmodel, linline, rinline, op)
    }
  } yield result

  private def inlineEqOp[S: Mangler: Exports: Arrows](
    lmodel: ValueModel,
    rmodel: ValueModel,
    linline: Inline,
    rinline: Inline,
    op: EqOp
  ): State[S, (ValueModel, Inline)] = (lmodel, rmodel) match {
    // Optimize in case compared values are literals
    // Semantics should check that types are comparable
    case (LiteralModel(lvalue, _), LiteralModel(rvalue, _)) =>
      (
        LiteralModel.bool {
          op match {
            case Eq => lvalue == rvalue
            case Neq => lvalue != rvalue
          }
        },
        linline.mergeWith(rinline, MergeMode.ParMode)
      ).pure[State[S, *]]
    case _ => fullInlineEqOp(lmodel, rmodel, linline, rinline, op)
  }

  private def fullInlineEqOp[S: Mangler: Exports: Arrows](
    lmodel: ValueModel,
    rmodel: ValueModel,
    linline: Inline,
    rinline: Inline,
    op: EqOp
  ): State[S, (ValueModel, Inline)] = {
    val (name, shouldMatch) = op match {
      case Eq => ("eq", true)
      case Neq => ("neq", false)
    }

    /**
     * (seq
     *   <left-inline>
     *   (seq
     *     <right-inline>
     *     (xor
     *       (match/mismatch <left-res> <right-res>
     *         (ap true <res-name>)
     *       )
     *       (ap false <res-name>)
     *     )
     *   )
     * )
     */
    val predo = (resName: String) =>
      SeqModel.wrap(
        linline.predo ++ rinline.predo :+ XorModel.wrap(
          // TODO: Canonicalize values if they are streams
          MatchMismatchModel(lmodel, rmodel, shouldMatch).wrap(
            FlattenModel(
              LiteralModel.bool(true),
              resName
            ).leaf
          ),
          FlattenModel(
            LiteralModel.bool(false),
            resName
          ).leaf
        )
      )

    result(name, predo)
  }

  private def inlineBoolOp[S: Mangler: Exports: Arrows](
    lmodel: ValueModel,
    rmodel: ValueModel,
    linline: Inline,
    rinline: Inline,
    op: BoolOp
  ): State[S, (ValueModel, Inline)] = (lmodel, rmodel) match {
    // Optimize in case of left value is known at compile time
    case (LiteralModel.Bool(lvalue), _) =>
      (op match {
        case And if !lvalue => (LiteralModel.bool(false), linline)
        case Or if lvalue => (LiteralModel.bool(true), linline)
        case _ => (rmodel, Inline(linline.predo ++ rinline.predo))
      }).pure[State[S, *]]
    // Optimize in case of right value is known at compile time and it has no computation
    case (_, LiteralModel.Bool(rvalue)) if rinline.predo.isEmpty =>
      (op match {
        case And if !rvalue => (LiteralModel.bool(false), linline)
        case Or if rvalue => (LiteralModel.bool(true), linline)
        case _ => (lmodel, linline)
      }).pure[State[S, *]]
    // Produce unoptimized inline
    case _ => fullInlineBoolOp(lmodel, rmodel, linline, rinline, op)
  }

  private def fullInlineBoolOp[S: Mangler: Exports: Arrows](
    lmodel: ValueModel,
    rmodel: ValueModel,
    linline: Inline,
    rinline: Inline,
    op: BoolOp
  ): State[S, (ValueModel, Inline)] = {
    val (name, compareWith) = op match {
      case And => ("and", false)
      case Or => ("or", true)
    }

    /**
     * (seq
     *   <left-inline>
     *   (xor
     *     (match <left-res> <compare-with>
     *       (ap <left-res> <res-name>)
     *     )
     *     (seq
     *       <right-inline>
     *       (ap <right-res> <res-name>)
     *     )
     *   )
     * )
     */
    val predo = (resName: String) =>
      SeqModel.wrap(
        linline.predo :+ XorModel.wrap(
          MatchMismatchModel(
            lmodel,
            LiteralModel.bool(compareWith),
            shouldMatch = true
          ).wrap(
            FlattenModel(
              lmodel,
              resName
            ).leaf
          ),
          SeqModel.wrap(
            rinline.predo :+ FlattenModel(
              rmodel,
              resName
            ).leaf
          )
        )
      )

    result(name, predo)
  }

  private def result[S: Mangler](
    name: String,
    predo: String => OpModel.Tree
  ): State[S, (ValueModel, Inline)] =
    Mangler[S]
      .findAndForbidName(name)
      .map(resName =>
        (
          VarModel(resName, ScalarType.bool),
          Inline(Chain.one(predo(resName)))
        )
      )
}
