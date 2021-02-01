package aqua

import cats.parse.{Parser ⇒ P, Parser0 ⇒ P0}

object Aqua {
 import Token._

  val `parser`: P0[List[Block]] = P.repSep0(Block.`block`, ` \n`)
}
