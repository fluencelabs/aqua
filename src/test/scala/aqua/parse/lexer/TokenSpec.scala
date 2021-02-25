package aqua.parse.lexer

import aqua.parse.lexer.Token._
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TokenSpec extends AnyFlatSpec with Matchers with EitherValues {

  "\\n token" should "be parsed" in {
    ` \n`.parseAll("\n") should be('right)
    ` \n`.parseAll(" \n") should be('right)
    ` \n`.parseAll("         \n") should be('right)
    ` \n`.parseAll("               \n") should be('right)
    ` \n`.parseAll("--comment\n") should be('right)
    ` \n`.parseAll(" --comment\n") should be('right)
    ` \n`.parseAll("    --comment\n") should be('right)
    ` \n`.parseAll("    --comment with many words\n") should be('right)
    ` \n`.parseAll("    --comment with many words      \n") should be('right)
    ` \n`.parse("    --comment with many words      \n").right.value should be(("", ()))
    ` \n`.parse("    --comment with many words      \n     ").right.value should be(("     ", ()))
  }

  "\\n* token" should "match the same strings" in {
    ` \n*`.parseAll("\n") should be('right)
    ` \n*`.parseAll(" \n") should be('right)
    ` \n*`.parseAll("         \n") should be('right)
    ` \n*`.parseAll("               \n") should be('right)
    ` \n*`.parseAll("--comment\n") should be('right)
    ` \n*`.parseAll(" --comment\n") should be('right)
    ` \n*`.parseAll("    --comment\n") should be('right)
    ` \n*`.parseAll("    --comment with many words\n") should be('right)
    ` \n*`.parseAll("    --comment with many words      \n") should be('right)
    ` \n*`.parse("    --comment with many words      \n").right.value should be(("", ()))
    ` \n*`.parse("    --comment with many words      \n     ").right.value should be(("     ", ()))
  }

  "\\n* token" should "match multi-line comments" in {
    ` \n*`.parseAll("""  -- comment line 1
                      |-- line 2
                      |
                      |              -- line 3
                      |              -- line 4
                      |""".stripMargin).right.value should be(())
  }

}
