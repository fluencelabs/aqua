package aqua.parser.lexer

import aqua.parser.lexer.Token._
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.parse.{Parser => P}

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
    ` \n+`.parseAll("\n") should be('right)
    ` \n+`.parseAll(" \n") should be('right)
    ` \n+`.parseAll("         \n") should be('right)
    ` \n+`.parseAll("               \n") should be('right)
    ` \n+`.parseAll("--comment\n") should be('right)
    ` \n+`.parseAll(" --comment\n") should be('right)
    ` \n+`.parseAll("    --comment\n") should be('right)
    ` \n+`.parseAll("    --comment with many words\n") should be('right)
    ` \n+`.parseAll("    --comment with many words      \n") should be('right)
    ` \n+`.parse("    --comment with many words      \n").right.value should be(("", ()))
    ` \n+`.parse("    --comment with many words      \n     ").right.value should be(("     ", ()))
  }

  "\\n* token" should "match multi-line comments" in {
    ` \n+`.parseAll("""  -- comment line 1
                      |-- line 2
                      |
                      |              -- line 3
                      |              -- line 4
                      |""".stripMargin).right.value should be(())
  }

  "indented" should "parse 1 or more lines" in {
    indented(_ => `.`, "").parseAll(" .\n .").right.value should be(NonEmptyList.of((), ()))
    indented(_ => `.`, "").parseAll(" .\n .\n .").right.value should be(NonEmptyList.of((), (), ()))
    indented(_ => `.`, "").parse(" .\n .\n .\n").right.value should be(("\n", NonEmptyList.of((), (), ())))
    indented(_ => `.`, "").parse(" .\n .\n .\n ").right.value should be(("\n ", NonEmptyList.of((), (), ())))
    indented(_ => `.`, "").parse(" .\n .\n .\n .").right.value should be(("", NonEmptyList.of((), (), (), ())))
    indented(_ => `.`, "").parseAll(" .").right.value should be(NonEmptyList.of(()))

    indented(_ => `.`, " ").parse("  .\n .").right.value should be(("\n .", NonEmptyList.of(())))
    indented(_ => `.`, " ").parse("  .\n  ").right.value should be(("\n  ", NonEmptyList.of(())))
  }

  "nested indented" should "not fail on empty lines" in {
    sealed trait Tree
    case object Leaf extends Tree
    case class Node(branches: NonEmptyList[Tree]) extends Tree

    lazy val p: P[NonEmptyList[Tree]] =
      indented(_ => P.string("newline") *> (` \n+` *> P.defer(p)).?.map(_.fold[Tree](Leaf)(Node)), "")
    p.parseAll(" newline").right.value should be(NonEmptyList.of(Leaf))
  }

}
