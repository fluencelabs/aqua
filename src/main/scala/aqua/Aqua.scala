package aqua

import aqua.parse.Block
import cats.parse.{Parser ⇒ P, Parser0 ⇒ P0}

object Aqua {
 import aqua.parse.Token._

  val `parser`: P0[List[Block]] = P.repSep0(Block.`block`, ` \n`)
}
