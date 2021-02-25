package aqua

import aqua.parse.{ArrowType, Block, DataType, Type}
import cats.Id
import cats.data.NonEmptyMap
import cats.parse.{Parser ⇒ P, Parser0 ⇒ P0}
import aqua.parse.lift.LiftParser.Implicits.idLiftParser

case class Aqua(
  inputs: Map[String, DataType],
  callbacks: Map[String, ArrowType],
  types: Map[String, Type],
  services: Map[String, NonEmptyMap[String, ArrowType]]
)

object Aqua {
  import aqua.parse.lexer.Token._

  val `parser`: P0[List[Block[Id]]] = P.repSep0(Block.`block`[Id], ` \n*`)
}
