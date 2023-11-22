package aqua.model.inline

import aqua.model.{EmptyModel, OpModel, ParModel, SeqModel}
import aqua.raw.ops.RawTag
import aqua.raw.value.ValueRaw
import aqua.model.inline.Inline.MergeMode
import aqua.model.inline.Inline.MergeMode.*

import cats.Monoid
import cats.data.Chain
import cats.data.Chain.*
import cats.syntax.option.*

import scala.collection.immutable.ListMap

/**
 * Inlining result
 *
 * @param predo operations tree
 * @param mergeMode how `predo` must be merged
 */
private[inline] case class Inline(
  predo: Chain[OpModel.Tree] = Chain.empty,
  mergeMode: MergeMode = ParMode
) {

  def append(tree: Option[OpModel.Tree]): Inline =
    tree match {
      case None => this
      case Some(tree) => copy(predo = predo :+ tree)
    }

  def prepend(tree: Option[OpModel.Tree]): Inline =
    tree match {
      case None => this
      case Some(tree) => copy(predo = tree +: predo)
    }

  def desugar: Inline = {
    val desugaredPredo = predo match {
      case Chain.nil | _ ==: Chain.nil => predo
      case chain =>
        mergeMode match
          case SeqMode =>
            val wrapped = SeqModel.wrap(chain)
            wrapped match
              case EmptyModel.leaf => Chain.empty
              case _ => Chain.one(wrapped)
          case ParMode => Chain.one(ParModel.wrap(chain))
    }

    Inline(desugaredPredo)
  }

  def mergeWith(inline: Inline, mode: MergeMode): Inline = {
    val left = desugar
    val right = inline.desugar

    Inline(left.predo ++ right.predo, mode)
  }
}

// TODO may not be needed there
private[inline] object Inline {

  enum MergeMode {
    case SeqMode
    case ParMode
  }

  val empty: Inline = Inline()

  def tree(tr: OpModel.Tree): Inline = Inline(predo = Chain.one(tr))

  given Monoid[Inline] with
    override val empty: Inline = Inline.empty

    override def combine(a: Inline, b: Inline): Inline =
      // TODO: Is it ok to ignore merge mode?
      Inline(a.predo ++ b.predo)

  def parDesugarPrefix(ops: List[OpModel.Tree]): Option[OpModel.Tree] =
    ops match {
      case Nil => none
      case x :: Nil => x.some
      case _ => ParModel.wrap(ops).some
    }

  def parDesugarPrefixOpt(ops: Option[OpModel.Tree]*): Option[OpModel.Tree] =
    parDesugarPrefix(ops.toList.flatten)
}
