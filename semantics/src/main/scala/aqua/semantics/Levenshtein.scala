package aqua.semantics

import scala.collection.mutable
import scala.collection.mutable
import cats.data.{Chain, NonEmptyList}

object Levenshtein {

  // copied from https://rosettacode.org/wiki/Levenshtein_distance#Scala
  def levenshtein(s1: String, s2: String): Int = {
    val memoizedCosts = mutable.Map[(Int, Int), Int]()

    def lev: ((Int, Int)) => Int = {
      case (k1, k2) =>
        memoizedCosts.getOrElseUpdate((k1, k2), (k1, k2) match {
          case (i, 0) => i
          case (0, j) => j
          case (i, j) =>
            Seq(1 + lev((i - 1, j)),
              1 + lev((i, j - 1)),
              lev((i - 1, j - 1))
                + (if (s1(i - 1) != s2(j - 1)) 1 else 0)).min
        })
    }

    lev((s1.length, s2.length))
    memoizedCosts(s1.length, s2.length)
  }

  // Get most similar to 'str' from list of strings
  def mostSimilar(str: String, possiblySimilar: NonEmptyList[String], count: Int) = {
    possiblySimilar.map(s => (s, levenshtein(str, s))).sortBy(_._2).take(count).map(_._1)
  }

  // TODO: add to a config?
  val SIMILAR_COUNT = 5

  // Generate a message based on similarity of strings
  def genMessage(prefix: String, str: String, possiblySimilar: List[String]) = {
    val possiblySimNel = NonEmptyList.fromList(possiblySimilar)
    possiblySimNel match {
      case Some(nel) =>
        val similar = mostSimilar(str, nel, SIMILAR_COUNT)
        s"$prefix. Did you mean any of this? ${similar.map(s => s"'$s'").mkString(", ")}"
      case None =>
        s"$prefix"
    }

  }

}
