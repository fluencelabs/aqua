package aqua.model.inline.raw

import aqua.errors.Errors.internalError
import aqua.model.*
import aqua.model.inline.raw.RawInliner
import aqua.model.inline.TagInliner
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{AbilityRaw, LiteralRaw, MakeStructRaw}
import cats.data.{NonEmptyList, NonEmptyMap, State}
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.{unfold, valueToModel}
import aqua.types.{ArrowType, ScalarType, Type}
import aqua.raw.value.ApplyBinaryOpRaw
import aqua.raw.value.ApplyBinaryOpRaw.Op
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

  override def apply[S: Mangler: Exports: Arrows](
    raw: ApplyBinaryOpRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = for {
    left <- unfold(raw.left, propertiesAllowed)
    (lmodel, linline) = left
    right <- unfold(raw.right, propertiesAllowed)
    (rmodel, rinline) = right

    result <- raw.op match {
      case op: Op.Bool =>
        inlineBoolOp(
          lmodel,
          rmodel,
          linline,
          rinline,
          op,
          raw.baseType
        )
      case op: Op.Eq =>
        for {
          // Canonicalize stream operands before comparison
          leftStream <- TagInliner.canonicalizeIfStream(lmodel)
          (lmodelStream, linlineStream) = leftStream.map(linline.append)
          rightStream <- TagInliner.canonicalizeIfStream(rmodel)
          (rmodelStream, rinlineStream) = rightStream.map(rinline.append)
          result <- inlineEqOp(
            lmodelStream,
            rmodelStream,
            linlineStream,
            rinlineStream,
            op,
            raw.baseType
          )
        } yield result
      case op: Op.Cmp =>
        inlineCmpOp(
          lmodel,
          rmodel,
          linline,
          rinline,
          op,
          raw.baseType
        )
      case op: Op.Math =>
        inlineMathOp(
          lmodel,
          rmodel,
          linline,
          rinline,
          op,
          raw.baseType
        )
    }
  } yield result

  private def inlineEqOp[S: Mangler: Exports: Arrows](
    lmodel: ValueModel,
    rmodel: ValueModel,
    linline: Inline,
    rinline: Inline,
    op: Op.Eq,
    resType: Type
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
    case _ => fullInlineEqOp(lmodel, rmodel, linline, rinline, op, resType)
  }

  private def fullInlineEqOp[S: Mangler: Exports: Arrows](
    lmodel: ValueModel,
    rmodel: ValueModel,
    linline: Inline,
    rinline: Inline,
    op: Op.Eq,
    resType: Type
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

    result(name, resType, predo)
  }

  private def inlineBoolOp[S: Mangler: Exports: Arrows](
    lmodel: ValueModel,
    rmodel: ValueModel,
    linline: Inline,
    rinline: Inline,
    op: Op.Bool,
    resType: Type
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
    case _ => fullInlineBoolOp(lmodel, rmodel, linline, rinline, op, resType)
  }

  private def fullInlineBoolOp[S: Mangler: Exports: Arrows](
    lmodel: ValueModel,
    rmodel: ValueModel,
    linline: Inline,
    rinline: Inline,
    op: Op.Bool,
    resType: Type
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

    result(name, resType, predo)
  }

  private def inlineCmpOp[S: Mangler: Exports: Arrows](
    lmodel: ValueModel,
    rmodel: ValueModel,
    linline: Inline,
    rinline: Inline,
    op: Op.Cmp,
    resType: Type
  ): State[S, (ValueModel, Inline)] = (lmodel, rmodel) match {
    case (LiteralModel.Integer(lv), LiteralModel.Integer(rv)) =>
      val res = op match {
        case Lt => lv < rv
        case Lte => lv <= rv
        case Gt => lv > rv
        case Gte => lv >= rv
      }

      (
        LiteralModel.bool(res),
        Inline(linline.predo ++ rinline.predo)
      ).pure
    case _ =>
      val fn = op match {
        case Lt => "lt"
        case Lte => "lte"
        case Gt => "gt"
        case Gte => "gte"
      }

      val predo = (resName: String) =>
        SeqModel.wrap(
          linline.predo ++ rinline.predo :+ CallServiceModel(
            serviceId = LiteralModel.quote("cmp"),
            funcName = fn,
            call = CallModel(
              args = lmodel :: rmodel :: Nil,
              exportTo = CallModel.Export(resName, resType) :: Nil
            )
          ).leaf
        )

      result(fn, resType, predo)
  }

  private def inlineMathOp[S: Mangler: Exports: Arrows](
    lmodel: ValueModel,
    rmodel: ValueModel,
    linline: Inline,
    rinline: Inline,
    op: Op.Math,
    resType: Type
  ): State[S, (ValueModel, Inline)] = (lmodel, rmodel) match {
    case (
          LiteralModel.Integer(lv),
          LiteralModel.Integer(rv)
        ) if !mathExceptionalCase(lv, rv, op) =>
      val res = op match {
        case Add => lv + rv
        case Sub => lv - rv
        case Mul => lv * rv
        case Div => lv / rv
        case Rem => lv % rv
        case Pow => intPow(lv, rv)
        case _ => internalError(s"Unsupported operation $op for $lv and $rv")
      }

      (
        LiteralModel.number(res),
        Inline(linline.predo ++ rinline.predo)
      ).pure
    case _ =>
      val fn = op match {
        case Add => "add"
        case Sub => "sub"
        case Mul => "mul"
        case FMul => "fmul"
        case Div => "div"
        case Rem => "rem"
        case Pow => "pow"
      }

      val predo = (resName: String) =>
        SeqModel.wrap(
          linline.predo ++ rinline.predo :+ CallServiceModel(
            serviceId = LiteralModel.quote("math"),
            funcName = fn,
            call = CallModel(
              args = lmodel :: rmodel :: Nil,
              exportTo = CallModel.Export(resName, resType) :: Nil
            )
          ).leaf
        )

      result(fn, resType, predo)
  }

  private def result[S: Mangler](
    name: String,
    resType: Type,
    predo: String => OpModel.Tree
  ): State[S, (ValueModel, Inline)] =
    Mangler[S]
      .findAndForbidName(name)
      .map(resName =>
        (
          VarModel(resName, resType),
          Inline(Chain.one(predo(resName)))
        )
      )

  private def mathExceptionalCase(
    left: Long,
    right: Long,
    op: Op.Math
  ): Boolean = op match {
    case Op.Div | Op.Rem => right == 0
    case Op.Pow => right < 0
    case _ => false
  }

  /**
   * Integer power
   *
   * @param base
   * @param exp >= 0
   * @return base ** exp
   */
  private def intPow(base: Long, exp: Long): Long = {
    def intPowTailRec(base: Long, exp: Long, acc: Long): Long =
      if (exp <= 0) acc
      else intPowTailRec(base * base, exp / 2, if (exp % 2 == 0) acc else acc * base)

    intPowTailRec(base, exp, 1)
  }
}
