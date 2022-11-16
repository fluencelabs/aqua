package aqua.model.inline

import aqua.model.{OpModel, ParModel, SeqModel}
import aqua.raw.ops.RawTag
import aqua.raw.value.ValueRaw
import cats.Monoid
import cats.data.Chain

sealed trait MergeMode
object SeqMode extends MergeMode
object ParMode extends MergeMode

/**
 *
 * @param flattenValues values that need to be resolved before `predo`
 * @param predo operations tree
 * @param mergeMode how `flattenValues` and `predo` must be merged
 */
private[inline] case class Inline(
  flattenValues: Map[String, ValueRaw] = Map.empty,
  predo: Chain[OpModel.Tree] = Chain.empty,
  mergeMode: MergeMode = ParMode
)

// TODO may not be needed there
private[inline] object Inline {
  val empty: Inline = Inline()

  def preload(pairs: (String, ValueRaw)*): Inline = Inline(pairs.toMap)

  def tree(tr: OpModel.Tree): Inline = Inline(predo = Chain.one(tr))

  given Monoid[Inline] with
    override val empty: Inline = Inline()

    override def combine(a: Inline, b: Inline): Inline =
      Inline(a.flattenValues ++ b.flattenValues, a.predo ++ b.predo)

  def parDesugarPrefix(ops: List[OpModel.Tree]): Option[OpModel.Tree] = ops match {
    case Nil => None
    case x :: Nil => Option(x)
    case _ => Option(ParModel.wrap(ops: _*))
  }

  def parDesugarPrefixOpt(ops: Option[OpModel.Tree]*): Option[OpModel.Tree] =
    parDesugarPrefix(ops.toList.flatten)
}
