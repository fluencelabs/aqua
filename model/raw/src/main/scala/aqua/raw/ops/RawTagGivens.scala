package aqua.raw.ops

import aqua.raw.value.LiteralRaw
import cats.free.Cofree
import cats.data.Chain
import cats.{Eval, Semigroup}
import cats.syntax.apply.*
import cats.syntax.semigroup.*

trait RawTagGivens {

  def isRightAssoc(tag: RawTag): Boolean = tag match {
    case XorTag | ParTag => true
    case _: XorParTag => true
    case _ => false
  }

  // convert some tags in tree to fix corner cases
  def fixCornerCases(tree: RawTag.Tree): RawTag.Tree =
    Cofree
      .cata[Chain, RawTag, RawTag.Tree](tree) {
        case (XorParTag(left, right), _) =>
          Eval.now(
            ParTag.wrap(
              XorTag.wrap(left),
              right
            )
          )
        case (XorTag.LeftBiased, tail) =>
          // TODO: fix me in topology
          // https://linear.app/fluence/issue/LNG-69/if-inside-on-produces-invalid-topology
          Eval.now(
            Cofree(
              XorTag,
              Eval.now(
                tail.append(
                  CallArrowRawTag.service(LiteralRaw.quote("op"), "noop", Call(Nil, Nil)).leaf
                )
              )
            )
          )
        case (head, tail) => Eval.now(Cofree(head, Eval.now(tail)))
      }
      .value

  given Semigroup[RawTag.Tree] with

    override def combine(x: RawTag.Tree, y: RawTag.Tree): RawTag.Tree = {
      // Remove right-asscoc protection of Seq with single child
      val flatX = SeqGroupTag.ungroupSingle(x)
      val flatY = SeqGroupTag.ungroupSingle(y)
      (flatX.head, flatY.head) match {
        case (_, XorParTag(xor, par)) => combine(combine(flatX, xor), par)
        case (XorParTag(xor, par), _) => combine(combine(xor, par), flatY)
        case (SeqTag, SeqTag) => flatY.copy(tail = (flatX.tail, flatY.tail).mapN(_ ++ _))
        case (_, SeqTag) => flatY.copy(tail = flatY.tail.map(_.prepend(flatX)))
        case (SeqTag, _) => flatX.copy(tail = flatX.tail.map(_.append(flatY)))
        case _ => SeqTag.wrap(flatX, flatY)
      }
    }

  // Semigroup for foldRight processing
  def rightAssocCombine(x: RawTag.Tree, y: RawTag.Tree): RawTag.Tree =
    (x.head, y.head) match {
      case (_: ParGroupTag, ParTag) =>
        y.copy(tail = (x.tail, y.tail).mapN(_ ++ _))
      case (XorTag, XorTag) =>
        y.copy(tail = (x.tail, y.tail).mapN(_ ++ _))
      case (XorTag.LeftBiased, XorTag) =>
        SeqGroupTag.wrap(y.copy(tail = (x.tail, y.tail).mapN(_ ++ _)))
      case (XorTag, ParTag) => XorParTag(x, y).leaf
      case (_, ParTag | XorTag) =>
        // When right-associative tag is combined with left-associative,
        // we need result to be left-associative to prevent greedy behavior.
        // SeqGroupTag does just this.
        SeqGroupTag.wrap(y.copy(tail = y.tail.map(_.prepend(x))))
      case (_, XorParTag(xor, par)) =>
        rightAssocCombine(rightAssocCombine(x, xor), par)
      case _ => x |+| y
    }

  extension (tree: RawTag.Tree)

    def toFuncOp: FuncOp = FuncOp(tree)

    def rename(vals: Map[String, String], declaredStreams: Set[String]): RawTag.Tree =
      println("rename: " + vals)
      if (vals.isEmpty) tree
      else
        tree.map[RawTag](_.mapValues(_.renameVars(vals, declaredStreams)).renameExports(vals))

    def renameExports(vals: Map[String, String]): RawTag.Tree =
      println("rename exports: " + vals)
      if (vals.isEmpty) tree
      else
        tree.map[RawTag](_.renameExports(vals))

    def definesVarNames: Eval[Set[String]] = 
      Cofree.cata[Chain, RawTag, Set[String]](tree) { case (tag, acc) =>
        Eval.later(acc.foldLeft(tag.definesVarNames)(_ ++ _))
      }
}
