package aqua.raw

import cats.Monoid
import cats.data.Chain
import aqua.types.Type

trait RawPart extends Raw {
  def name: String

  def rawPartType: Type

  def rename(s: String): RawPart
}

object RawPart {

  case class Parts(parts: Chain[RawPart]) extends Raw {

    def collectMap[T](f: PartialFunction[RawPart, T]): Map[String, T] =
      parts.collect {
        case rp if f.isDefinedAt(rp) => rp.name -> f(rp)
      }.toList.toMap

    def pick(name: String, rename: Option[String]): Parts =
      Parts(parts.filter(_.name == name).map(rp => rename.fold(rp)(rp.rename)))
  }

  implicit object RPSMonoid extends Monoid[Parts] {
    override def empty: Parts = Parts(Chain.empty)

    override def combine(x: Parts, y: Parts): Parts =
      Parts(
        x.parts ++ y.parts
      )
  }

  def contextPart(raw: Raw): Parts = raw match {
    case cr: Parts => cr
    case _ =>
      Parts(Chain.one(raw).collect { case rp: RawPart =>
        rp
      })
  }

}
