package aqua.parser

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits._
import cats.Id

class BlockSpec extends AnyFlatSpec with Matchers with EitherValues {

  "blocks list" should "parse" in {
    Block.blocks[Id].parseAll("""func some(cb: -> i32):
                                |  cb()
                                |  
                                |func other(cb: -> i32):
                                |  on 22:
                                |    cb()
                                |  cb()
                                |  
                                |func third(cb: -> i32):
                                |  on 23:
                                |    cb()
                                |    
                                |    
                                |  
                                |""".stripMargin).right.value.length should be(3)
  }

}
