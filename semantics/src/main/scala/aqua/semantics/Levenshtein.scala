package aqua.semantics

import cats.data.{Chain, NonEmptyList}

import scala.collection.mutable

object Levenshtein {

  def countDistance(str1: String, str2: String): Int = {
    val allDistances = mutable.Map[(Int, Int), Int]()

    def distance(left: Int, right: Int): Int = {
      allDistances.getOrElseUpdate(
        (left, right),
        (left, right) match {
          case (i, 0) => i
          case (0, j) => j
          case (i, j) =>
            Set(
              1 + distance(i - 1, j),
              1 + distance(i, j - 1),
              distance(i - 1, j - 1)
                + (if (str1(i - 1) != str2(j - 1)) 1 else 0)
            ).min
        }
      )
    }

    distance(str1.length, str2.length)

    // get last distance
    allDistances(str1.length, str2.length)
  }

  // Get most similar to 'str' from list of strings
  def mostSimilar(str: String, possiblySimilar: NonEmptyList[String], count: Int) = {
    possiblySimilar.map(s => (s, countDistance(str, s))).sortBy(_._2).take(count).map(_._1)
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
