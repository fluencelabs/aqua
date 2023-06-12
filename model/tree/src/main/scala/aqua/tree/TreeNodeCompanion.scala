package aqua.tree

import cats.Show
import cats.data.Chain
import cats.free.Cofree

import cats.syntax.show.*
import cats.syntax.apply.*

import scala.annotation.tailrec

trait TreeNodeCompanion[T <: TreeNode[T]] {

  given showTreeLabel: Show[T]

  type Tree = Cofree[Chain, T]

  private def showOffset(what: Tree, offset: Int): String = {
    val spaces = "| " * offset
    spaces + what.head.show + what.tail.map {
      case ch if ch.nonEmpty =>
        " :\n" + ch.toList.map(showOffset(_, offset + 1)).mkString("")
      case ch => "\n"
    }.value
  }

  private def showDiffOffset(what: (Tree, Tree), offset: Int): String = {
    val spaces = "| " * offset
    val head =
      if (what._1.head == what._2.head) what._1.head.show
      else {
        val lft = what._1.head.show
        val rgt = what._2.head.show
        val commonPrefixLen = lft.zip(rgt).takeWhile(_ == _).length
        val commonSuffixLen = rgt.reverse.zip(lft.reverse).takeWhile(_ == _).length
        val commonPrefix = lft.take(commonPrefixLen)
        val commonSuffix = rgt.takeRight(commonSuffixLen)
        val lSuffix = lft.length - commonSuffixLen
        val lftDiff =
          if (commonPrefixLen - lSuffix < lft.length) lft.substring(commonPrefixLen, lSuffix)
          else ""
        val rSuffix = rgt.length - commonSuffixLen
        val rgtDiff =
          if (commonPrefixLen + rSuffix < rgt.length) rgt.substring(commonPrefixLen, rSuffix)
          else ""
        if (rgtDiff.isEmpty) {
          commonPrefix + Console.YELLOW + lftDiff + Console.RESET + commonSuffix
        } else {
          commonPrefix +
            Console.YELLOW + lftDiff + Console.RED + " != " + Console.CYAN + rgtDiff + Console.RESET + commonSuffix
        }

      }

    spaces + head + (what._1.tail, what._2.tail).mapN {
      case (c1, c2) if c1.isEmpty && c2.isEmpty => "\n"
      case (c1, c2) =>
        @tailrec
        def nxt(l: List[Tree], r: List[Tree], acc: Chain[String]): List[String] = (l, r) match {
          case (x :: tail, Nil) =>
            nxt(tail, Nil, acc :+ (Console.YELLOW + showOffset(x, offset + 1) + Console.RESET))
          case (Nil, y :: tail) =>
            nxt(tail, Nil, acc :+ (Console.CYAN + showOffset(y, offset + 1) + Console.RESET))
          case (x :: xt, y :: yt) if x.head == y.head =>
            nxt(xt, yt, acc :+ showDiffOffset(x -> y, offset + 1))
          case (x :: xt, yt) if yt.exists(_.head == x.head) =>
            val yh = yt.takeWhile(_.head != x.head)
            nxt(
              l,
              yt.drop(yh.length),
              acc ++ Chain.fromSeq(
                yh.map(y => Console.CYAN + showOffset(y, offset + 1) + Console.RESET)
              )
            )
          case (xt, y :: yt) if xt.exists(_.head == y.head) =>
            val xh = xt.takeWhile(_.head != y.head)
            nxt(
              xt.drop(xh.length),
              r,
              acc ++ Chain.fromSeq(
                xh.map(x => Console.YELLOW + showOffset(x, offset + 1) + Console.RESET)
              )
            )
          case (x :: xt, y :: yt) =>
            nxt(xt, yt, acc :+ showDiffOffset(x -> y, offset + 1))
          case (Nil, Nil) => acc.toList
        }

        " :" + (if (c1.length == c2.length) "\n"
                else
                  s" ${Console.YELLOW}given ${c1.size}${Console.RED} != ${Console.CYAN}expected ${c2.size}${Console.RESET}\n") + nxt(
          c1.toList,
          c2.toList,
          Chain.empty
        ).mkString("")

    }.value
  }

  given Show[Tree] with

    override def show(t: Tree): String =
      showOffset(t, 0)

  given Show[(Tree, Tree)] with

    override def show(tt: (Tree, Tree)): String =
      showDiffOffset(tt, 0)

  extension (t: Tree)

    def equalsOrShowDiff(other: Tree): Boolean =
      if (t.forceAll == other.forceAll) true
      else {
        println((t, other).show)
        false
      }
}
