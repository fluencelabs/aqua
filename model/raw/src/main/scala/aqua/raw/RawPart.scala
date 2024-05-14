package aqua.raw

import aqua.types.Type

import cats.Monoid
import cats.data.Chain

trait RawPart extends Raw {
  def name: String

  def rawPartType: Type

  def rename(s: String): RawPart

  def addAbilityName(s: String): RawPart
}

object RawPart {

  case class Parts(parts: Chain[RawPart]) extends Raw

  given Monoid[Parts] with {
    override def empty: Parts = Parts(Chain.empty)

    override def combine(x: Parts, y: Parts): Parts =
      Parts(x.parts ++ y.parts)
  }

  def contextPart(raw: Raw): Parts = raw match {
    case cr: Parts => cr
    case _ =>
      Parts(Chain.one(raw).collect { case rp: RawPart =>
        rp
      })
  }

}
