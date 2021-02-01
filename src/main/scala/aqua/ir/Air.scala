package aqua.ir

import cats.Show
import cats.syntax.show._

abstract sealed class Keyword(val value: String)

object Keyword{
  case object Null extends Keyword("null")
  case object Next extends Keyword("next")
  case object Fold extends Keyword("fold")
  case object Match extends Keyword("match")
  case object Call extends Keyword("call")
  case object Seq extends Keyword("seq")
  case object Par extends Keyword("par")
  case object Xor extends Keyword("xor")
}

case class Triplet(peerId: String, serviceId: String, functionName: String)

  abstract sealed class Air(val keyword: Keyword)

object Air {

  case object Null extends Air(Keyword.Null)

  case class Next(label: String) extends Air(Keyword.Next)

  case class Fold(iterable: String, label: String, instruction: Air) extends Air(Keyword.Fold)

  case class Match(left: String, right: String, instruction: Air) extends Air(Keyword.Match)

  case class Par(left: Air, right: Air) extends Air(Keyword.Par)

  case class Seq(left: Air, right: Air) extends Air(Keyword.Seq)

  case class Xor(left: Air, right: Air) extends Air(Keyword.Xor)

  case class Call(triplet: Triplet, args: List[String], result: Option[String]) extends Air(Keyword.Call)


  private def show(depth: Int, air: Air): String = {
    def showNext(a: Air) = show(depth + 1, a)

    val space = "\t" * depth
    s"$space(${air.keyword.value}" +
      (air match {
        case Air.Null ⇒ ""
        case Air.Next(label) ⇒ s" $label"
        case Air.Fold(iter, label, inst) ⇒ s" $iter $label\n${showNext(inst)}$space"
        case Air.Match(left, right, inst) ⇒ s" $left $right\n${showNext(inst)}$space"
        case Air.Par(l, r) ⇒ s"\n${showNext(l)}${showNext(r)}$space"
        case Air.Seq(l, r) ⇒ s"\n${showNext(l)}${showNext(r)}$space"
        case Air.Xor(l, r) ⇒ s"\n${showNext(l)}${showNext(r)}$space"
        case Air.Call(Triplet(p, s, f), args, res) ⇒ s" $p ($s $f) [${args.mkString(", ")}]${res.fold("")(" " + _)}"
      }) + ")\n"
  }


  implicit val s: Show[Air] = Show.show(show(0, _))
}