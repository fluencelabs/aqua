package aqua.backend.air

import cats.Show
import cats.syntax.show._

abstract sealed class Keyword(val value: String)

object Keyword {
  case object NA extends Keyword("")

  case object Null extends Keyword("null")

  case object Next extends Keyword("next")

  case object Fold extends Keyword("fold")

  case object Match extends Keyword("match")

  case object Mismatch extends Keyword("mismatch")

  case object Call extends Keyword("call")

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

  case class Stream(name: String) extends DataView

  case class VarLens(name: String, lens: String) extends DataView {
    def append(sublens: String): VarLens = copy(lens = lens + sublens)
  }

  implicit val show: Show[DataView] = Show.show {
    case StringScalar(v) ⇒ v
    case InitPeerId ⇒ "%init_peer_id%"
    case LastError ⇒ "%last_error%"
    case Variable(name) ⇒ name
    case Stream(name) ⇒ name
    case VarLens(name, lens) ⇒ name + ".$" + lens + "!"
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

  case class Next(label: String) extends Air(Keyword.Next)

  case class Fold(iterable: DataView, label: String, instruction: Air) extends Air(Keyword.Fold)

  case class Match(left: DataView, right: DataView, instruction: Air) extends Air(Keyword.Match)

  case class Mismatch(left: DataView, right: DataView, instruction: Air)
      extends Air(Keyword.Mismatch)

  case class Par(left: Air, right: Air) extends Air(Keyword.Par)

  case class Seq(left: Air, right: Air) extends Air(Keyword.Seq)

  case class Xor(left: Air, right: Air) extends Air(Keyword.Xor)

  case class Call(triplet: Triplet, args: List[DataView], result: Option[String])
      extends Air(Keyword.Call)

  case class Comment(comment: String, air: Air) extends Air(Keyword.NA)

  private def show(depth: Int, air: Air): String = {
    def showNext(a: Air) = show(depth + 1, a)

    val space = " " * depth

    air match {
      case Air.Comment(c, a) =>
        space + "; " + c.replace("\n", "\n" + space + "; ") + "\n" +
          show(depth, a)
      case _ =>
        s"$space(${air.keyword.value}" +
          (air match {
            case Air.Null ⇒ ""
            case Air.Next(label) ⇒ s" $label"
            case Air.Fold(iter, label, inst) ⇒ s" ${iter.show} $label\n${showNext(inst)}$space"
            case Air.Match(left, right, inst) ⇒
              s" ${left.show} ${right.show}\n${showNext(inst)}$space"
            case Air.Mismatch(left, right, inst) ⇒
              s" ${left.show} ${right.show}\n${showNext(inst)}$space"
            case Air.Par(l, r) ⇒ s"\n${showNext(l)}${showNext(r)}$space"
            case Air.Seq(l, r) ⇒ s"\n${showNext(l)}${showNext(r)}$space"
            case Air.Xor(l, r) ⇒ s"\n${showNext(l)}${showNext(r)}$space"
            case Air.Call(triplet, args, res) ⇒
              s" ${triplet.show} [${args.map(_.show).mkString(" ")}]${res.fold("")(" " + _)}"
            case Air.Comment(_, _) => ";; Should not be displayed"
          }) + ")\n"
    }

  }

  implicit val s: Show[Air] = Show.show(show(0, _))
}
