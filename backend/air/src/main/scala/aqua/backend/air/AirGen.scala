/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.backend.air

import aqua.model.*
import aqua.raw.ops.Call
import aqua.res.*
import aqua.types.{ArrayType, CanonStreamMapType, CanonStreamType, StreamMapType, StreamType, Type}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import scribe.Logging

sealed trait AirGen {
  def generate: Air

}

object AirGen extends Logging {

  def propertyToString(ls: List[PropertyModel]): String = ls match {
    case Nil => ""
    case FunctorModel(field, _) :: tail =>
      s".$field${propertyToString(tail)}"
    case IntoFieldModel(field, _) :: tail =>
      s".$field${propertyToString(tail)}"
    case IntoIndexModel(idx, _) :: tail =>
      s".[$idx]${propertyToString(tail)}"
  }

  def varNameToString(name: String, `type`: Type): String =
    (`type` match {
      case _: StreamType => "$" + name
      case _: CanonStreamType => "#$" + name
      case _: CanonStreamMapType => "#%" + name
      case _: StreamMapType => "%" + name
      case _ => name
    }).replace('.', '_')

  def valueToData(vm: ValueModel): DataView = vm match {
    case LiteralModel(value, _) => DataView.StringScalar(value)
    case VarModel(name, t, property) =>
      val n = varNameToString(name, t)
      if (property.isEmpty) DataView.Variable(n)
      else {
        val functors = property.find {
          case FunctorModel(_, _) => true
          case _ => false
        }
        DataView.VarLens(n, propertyToString(property.toList), functors.isEmpty)
      }
  }

  def opsToSingle(ops: Chain[AirGen]): AirGen = ops.toList match {
    case Nil => NullGen
    case h :: Nil => h
    case list => list.reduceLeft(SeqGen(_, _))
  }

  def exportToString(exportTo: CallModel.Export): String = (exportTo match {
    case CallModel.Export(name, t) => varNameToString(name, t)
  })

  private def folder(op: ResolvedOp, ops: Chain[AirGen]): Eval[AirGen] =
    op match {
//      case mt: MetaTag =>
//        folder(mt.op, ops).map(ag => mt.comment.fold(ag)(CommentGen(_, ag)))
      case SeqRes =>
        Eval later ops.toList.reduceLeftOption(SeqGen(_, _)).getOrElse(NullGen)
      case ParRes =>
        Eval later (ops.toList match {
          case o :: Nil => ParGen(o, NullGen)
          case _ =>
            ops.toList.reduceLeftOption(ParGen(_, _)).getOrElse {
              logger.warn("ParRes with no children converted to Null")
              NullGen
            }
        })
      case XorRes =>
        Eval later (ops.toList match {
          case o :: Nil => XorGen(o, NullGen)
          case _ =>
            ops.toList.reduceLeftOption(XorGen(_, _)).getOrElse {
              logger.warn("XorRes with no children converted to Null")
              NullGen
            }
        })

      case NextRes(item) =>
        Eval later NextGen(item)
      case MatchMismatchRes(left, right, shouldMatch) =>
        Eval later MatchMismatchGen(
          valueToData(left),
          valueToData(right),
          shouldMatch,
          opsToSingle(ops)
        )

      case FoldRes(item, iterable, mode) =>
        val m = mode match {
          case FoldRes.Mode.Null => NullGen
          case FoldRes.Mode.Never => NeverGen
        }
        Eval later ForGen(valueToData(iterable), item, opsToSingle(ops), m)
      case RestrictionRes(item, itemType) =>
        Eval later NewGen(varNameToString(item, itemType), opsToSingle(ops))
      case CallServiceRes(serviceId, funcName, CallRes(args, exportTo), peerId) =>
        Eval.later(
          ServiceCallGen(
            valueToData(peerId),
            valueToData(serviceId),
            funcName,
            args.map(valueToData),
            exportTo.map(exportToString)
          )
        )

      case ApStreamMapRes(key, value, exportTo) =>
        Eval.later(
          ApStreamMapGen(valueToData(key), valueToData(value), exportToString(exportTo))
        )
      case ApRes(operand, exportTo) =>
        Eval.later(
          ApGen(valueToData(operand), exportToString(exportTo))
        )

      case FailRes(operand) =>
        Eval.later(
          FailGen(valueToData(operand))
        )

      case CanonRes(operand, peerId, exportTo) =>
        Eval.later(
          CanonGen(valueToData(operand), valueToData(peerId), exportToString(exportTo))
        )

      case NullRes =>
        Eval.now(NullGen)

      case _: NoAir =>
        Eval later NullGen

    }

  def apply(op: Cofree[Chain, ResolvedOp]): AirGen =
    Cofree
      .cata[Chain, ResolvedOp, AirGen](op)(folder)
      .value
}

case object NullGen extends AirGen {
  override def generate: Air = Air.Null
}

case object NeverGen extends AirGen {
  override def generate: Air = Air.Never
}

case class SeqGen(left: AirGen, right: AirGen) extends AirGen {

  override def generate: Air =
    Air.Seq(left.generate, right.generate)

}

case class CommentGen(comment: String, op: AirGen) extends AirGen {

  override def generate: Air =
    Air.Comment(comment, op.generate)
}

case class ApStreamMapGen(key: DataView, operand: DataView, result: String) extends AirGen {

  override def generate: Air =
    Air.ApStreamMap(key, operand, result)
}

case class ApGen(operand: DataView, result: String) extends AirGen {

  override def generate: Air =
    Air.Ap(operand, result)
}

case class FailGen(operand: DataView) extends AirGen {

  override def generate: Air =
    Air.Fail(operand)
}

case class CanonGen(operand: DataView, peerId: DataView, result: String) extends AirGen {

  override def generate: Air =
    Air.Canon(operand, peerId, result)
}

case class MatchMismatchGen(
  left: DataView,
  right: DataView,
  shouldMatch: Boolean,
  body: AirGen
) extends AirGen {

  override def generate: Air =
    if (shouldMatch) Air.Match(left, right, body.generate)
    else Air.Mismatch(left, right, body.generate)
}

case class ForGen(iterable: DataView, item: String, body: AirGen, mode: AirGen) extends AirGen {
  override def generate: Air = Air.Fold(iterable, item, body.generate, mode.generate)
}

case class NewGen(name: String, body: AirGen) extends AirGen {

  override def generate: Air = Air.New(
    DataView.Variable(name),
    body.generate
  )
}

case class NextGen(item: String) extends AirGen {
  override def generate: Air = Air.Next(item)
}

case class ServiceCallGen(
  peerId: DataView,
  srvId: DataView,
  fnName: String,
  args: List[DataView],
  result: Option[String]
) extends AirGen {

  override def generate: Air =
    Air.Call(
      Triplet.Full(peerId, srvId, fnName),
      args,
      result
    )
}

case class ParGen(left: AirGen, right: AirGen) extends AirGen {

  override def generate: Air =
    Air.Par(left.generate, right.generate)
}

case class XorGen(left: AirGen, right: AirGen) extends AirGen {

  override def generate: Air =
    Air.Xor(left.generate, right.generate)
}
