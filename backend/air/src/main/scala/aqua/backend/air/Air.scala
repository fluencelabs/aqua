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

import cats.Show
import cats.syntax.show.*

abstract sealed class Keyword(val value: String)

object Keyword {
  case object NA extends Keyword("")

  case object Null extends Keyword("null")
  case object Never extends Keyword("never")

  case object New extends Keyword("new")
  case object Next extends Keyword("next")

  case object Fold extends Keyword("fold")

  case object Match extends Keyword("match")

  case object Mismatch extends Keyword("mismatch")

  case object Call extends Keyword("call")

  case object Ap extends Keyword("ap")

  case object Fail extends Keyword("fail")

  case object Canon extends Keyword("canon")

  case object Seq extends Keyword("seq")

  case object Par extends Keyword("par")

  case object Xor extends Keyword("xor")

}

abstract sealed class DataView

object DataView {

  case class StringScalar(value: String) extends DataView

  case object InitPeerId extends DataView

  case object LastError extends DataView

  case class Variable(name: String) extends DataView

  case class VarLens(name: String, lens: String, isField: Boolean = true) extends DataView {
    def append(sublens: String): VarLens = copy(lens = lens + sublens)
  }

  implicit val show: Show[DataView] = Show.show {
    case StringScalar(v) ⇒ v
    case InitPeerId ⇒ "%init_peer_id%"
    case LastError ⇒ "%last_error%"
    case Variable(name) ⇒ name
    case VarLens(name, lens, isField) ⇒
      if (isField) name + ".$" + lens
      else name + lens
  }
}

abstract sealed class Triplet

object Triplet {

  case class FromData(peerAndService: DataView, functionName: String) extends Triplet

  case class Full(peerId: DataView, serviceId: DataView, functionName: String) extends Triplet

  implicit val show: Show[Triplet] = Show.show {
    case FromData(ps, fn) ⇒ s"${ps.show} " + "\"" + fn + "\""
    case Full(p, s, fn) ⇒ s"${p.show} (${s.show} " + "\"" + fn + "\")"
  }
}

abstract sealed class Air(val keyword: Keyword)

object Air {

  case object Null extends Air(Keyword.Null)

  case object Never extends Air(Keyword.Never)

  case class New(item: DataView, instruction: Air) extends Air(Keyword.New)

  case class Next(label: String) extends Air(Keyword.Next)

  case class Fold(
    iterable: DataView,
    label: String,
    instruction: Air,
    lastNextInstruction: Air
  ) extends Air(Keyword.Fold)

  case class Match(left: DataView, right: DataView, instruction: Air) extends Air(Keyword.Match)

  case class Mismatch(left: DataView, right: DataView, instruction: Air)
      extends Air(Keyword.Mismatch)

  case class Par(left: Air, right: Air) extends Air(Keyword.Par)

  case class Seq(left: Air, right: Air) extends Air(Keyword.Seq)

  case class Xor(left: Air, right: Air) extends Air(Keyword.Xor)

  case class Call(triplet: Triplet, args: List[DataView], result: Option[String])
      extends Air(Keyword.Call)

  case class ApStreamMap(key: DataView, op: DataView, result: String) extends Air(Keyword.Ap)

  case class Ap(op: DataView, result: String) extends Air(Keyword.Ap)

  case class Fail(op: DataView) extends Air(Keyword.Fail)

  case class Canon(op: DataView, peerId: DataView, result: String) extends Air(Keyword.Canon)

  case class Comment(comment: String, air: Air) extends Air(Keyword.NA)

  private def showInternal(space: String, sb: StringBuilder, air: Air): Unit = {

    def showNext(a: Air): Unit = showInternal(space + " ", sb, a)

    air match {
      case Air.Comment(c, a) =>
        sb.append(space)
          .append("; ")
          .append(c.replace("\n", "\n" + space + "; "))
          .append("\n")

        showInternal(space, sb, a)

      case _ =>
        sb.append(s"$space(${air.keyword.value}")
        (air match {
          case Air.Null ⇒
          case Air.Never ⇒
          case Air.Next(label) ⇒ sb.append(s" $label")
          case Air.New(item, inst) ⇒
            sb.append(s" ${item.show}\n")
            showNext(inst)
            sb.append(space)
          case Air.Fold(iter, label, inst, lastInst) ⇒
            sb.append(s" ${iter.show} $label\n")
            showNext(inst)
            showNext(lastInst)
            sb.append(space)
          case Air.Match(left, right, inst) ⇒
            sb.append(s" ${left.show} ${right.show}\n")
            showNext(inst)
            sb.append(space)
          case Air.Mismatch(left, right, inst) ⇒
            sb.append(s" ${left.show} ${right.show}\n")
            showNext(inst)
            sb.append(space)
          case Air.Par(l, r) ⇒
            sb.append("\n")
            showNext(l)
            showNext(r)
            sb.append(space)
          case Air.Seq(l, r) ⇒
            sb.append("\n")
            showNext(l)
            showNext(r)
            sb.append(space)
          case Air.Xor(l, r) ⇒
            sb.append("\n")
            showNext(l)
            showNext(r)
            sb.append(space)
          case Air.Call(triplet, args, res) ⇒
            sb.append(s" ${triplet.show} [${args.map(_.show).mkString(" ")}]${res.fold("")(" " + _)}")
          case Air.Ap(operand, result) ⇒
            sb.append(s" ${operand.show} $result")
          case Air.ApStreamMap(key, operand, result) ⇒
            sb.append(s" (${key.show} ${operand.show}) $result")
          case Air.Fail(operand) => sb.append(s" ${operand.show}")
          case Air.Canon(operand, peerId, result) ⇒
            sb.append(s" ${peerId.show} ${operand.show}  $result")
          case Air.Comment(_, _) => ";; Should not be displayed"
        })
        sb.append(")\n")
    }
  }

  private def show(depth: Int, air: Air): String = {
    val sb = StringBuilder()
    val space = " " * depth
    showInternal(space, sb, air)
    sb.result()
  }

  implicit val s: Show[Air] = Show.show(show(0, _))
}
