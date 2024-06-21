/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.semantics

import cats.data.{Chain, NonEmptyList}

import scala.collection.mutable

object Levenshtein {

  def calculateDistance(str1: String, str2: String): Int = {
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
    possiblySimilar.map(s => (s, calculateDistance(str, s))).sortBy(_._2).take(count).map(_._1)
  }

  // TODO: add to a config?
  val SIMILARITY_THRESHOLD = 5

  // Generate a message based on similarity of strings
  def genMessage(prefix: String, str: String, possiblySimilar: List[String]) = {
    val possiblySimNel = NonEmptyList.fromList(possiblySimilar)
    possiblySimNel match {
      case Some(nel) =>
        val similar = mostSimilar(str, nel, SIMILARITY_THRESHOLD)
        s"$prefix. Did you mean any of this? ${similar.map(s => s"'$s'").mkString(", ")}"
      case None =>
        s"$prefix"
    }

  }

}
