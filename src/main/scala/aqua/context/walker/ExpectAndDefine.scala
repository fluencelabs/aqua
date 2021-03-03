package aqua.context.walker

import aqua.context.marker.Marker
import aqua.context.scope.{Mode, ParMode, XorMode}
import aqua.parser.lexer.Token

case class ExpectAndDefine[In, Out](
  expectAcc: Acc[In],
  defineAcc: Map[String, Out]
) {
  type Self = ExpectAndDefine[In, Out]

  def combine(other: Self, mode: Option[Mode]): Self =
    mode match {
      case None => combineSeq(other)
      case Some(XorMode) => combineXor(other)
      case Some(ParMode) => combinePar(other)
    }

  def combineSeq(other: Self): Self =
    copy(expectAcc = expectAcc.add(other.expectAcc, defineAcc.keySet), defineAcc = defineAcc ++ other.defineAcc)

  def combinePar(other: Self): Self =
    copy(expectAcc = expectAcc add other.expectAcc, defineAcc = defineAcc ++ other.defineAcc)

  def combineXor(other: Self): Self =
    copy(expectAcc = expectAcc add other.expectAcc)

  def expect(addition: Acc[In]): Self =
    copy(expectAcc = expectAcc add addition)

  def resolved(rem: String): Self =
    copy(expectAcc = expectAcc sub rem)

  def defined(k: String, v: Out): Self =
    copy(defineAcc = defineAcc + (k -> v))

  def collectDefinitions(pf: PartialFunction[Out, Out]): Self =
    copy(defineAcc = defineAcc.filter {
      case (_, v) if pf.isDefinedAt(v) => true
      case _ => false
    })

  def undefine(rem: String): Self =
    copy(defineAcc = defineAcc - rem)

  def clearDefinitions: Self = copy(defineAcc = Map.empty[String, Out])
  def clearExpectations: Self = copy(expectAcc = expectAcc.erase)
}

object ExpectAndDefine {

  def empty[F[_], In <: Token[F], Out <: Marker[F]]: ExpectAndDefine[In, Out] =
    ExpectAndDefine(Acc.empty[In], Map.empty[String, Out])
}
