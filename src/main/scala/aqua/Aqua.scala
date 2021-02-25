package aqua

import aqua.parse.{ArrowType, Block, DataType, Type}
import cats.data.NonEmptyMap
import cats.parse.{Parser ⇒ P, Parser0 ⇒ P0}

case class Aqua(
  inputs: Map[String, DataType],
  callbacks: Map[String, ArrowType],
  types: Map[String, Type],
  services: Map[String, NonEmptyMap[String, ArrowType]]
)

object Aqua {
  import aqua.parse.Token._

  val `parser`: P0[List[Block]] = P.repSep0(Block.`block`, ` \n*`)
}
