package aqua.model.inline

import aqua.mangler.ManglerState
import aqua.model.inline.state.Mangler
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ManglerSpec extends AnyFlatSpec with Matchers {

  "mangler" should "forbid right" in {
    val mangler = Mangler.Simple

    val a = for {
      _ <- mangler.forbid(Set("first", "second"))
      fn <- mangler.getForbiddenNames
    } yield fn

    println(a.runA(ManglerState()).value)
  }

  "mangler" should "rename right" in {
    val mangler = Mangler.Simple

    val results = for {
      res <- mangler.findAndForbidNames(Set("first", "second"))
      fn <- mangler.getForbiddenNames
    } yield (res, fn)

    val (res, _) = results.runA(ManglerState()).value
    res shouldBe Map()
  }

  "mangler" should "rename same words right" in {
    val mangler = Mangler.Simple

    val results = for {
      res1 <- mangler.findAndForbidNames(Set("first", "first"))
      res2 <- mangler.findAndForbidNames(Set("first", "first"))
      fn <- mangler.getForbiddenNames
    } yield (res1, res2, fn)

    val (r1, r2, _) = results.runA(ManglerState()).value
    r1 shouldBe Map()
    r2 shouldBe Map("first" -> "first-0")
  }

  "mangler" should "rename right if already have renamed" in {
    val mangler = Mangler.Simple

    val results = for {
      res1 <- mangler.findAndForbidNames(Set("first", "first-0", "first-1"))
      res2 <- mangler.findAndForbidNames(Set("first"))
      res3 <- mangler.findAndForbidNames(Set("first-0"))
      res4 <- mangler.findAndForbidNames(Set("first-1"))
      res5 <- mangler.findAndForbidNames(Set("first-2"))
      fn <- mangler.getForbiddenNames
    } yield (res1, res2, res3, res4, res5, fn)

    val (r1, r2, r3, r4, r5, _) = results.runA(ManglerState()).value
    r1 shouldBe Map()
    r2 shouldBe Map("first" -> "first-2")
    r3 shouldBe Map("first" -> "first-0-0")
    r4 shouldBe Map("first" -> "first-1-0")
    r5 shouldBe Map("first" -> "first-2-0")
  }

  "mangler" should "rename multiple times right" in {
    val mangler = Mangler.Simple

    val results = for {
      res <- mangler.findAndForbidNames(Set("first", "second"))
      res2 <- mangler.findAndForbidNames(Set("first", "second"))
      res3 <- mangler.findAndForbidNames(Set("first"))
      res4 <- mangler.findAndForbidNames(Set("first", "second"))
      res5 <- mangler.findAndForbidNames(Set("second"))
      fn <- mangler.getForbiddenNames
    } yield (res, res2, res3, res4, res5, fn)

    val (r1, r2, r3, r4, r5, _) = results.runA(ManglerState()).value
    r1 shouldBe Map()
    r2 shouldBe Map("first" -> "first-0", "second" -> "second-0")
    r3 shouldBe Map("first" -> "first-1")
    r4 shouldBe Map("first" -> "first-2", "second" -> "second-1")
    r5 shouldBe Map("second" -> "second-2")
  }

  "mangler" should "forbid and rename right" in {
    val mangler = Mangler.Simple

    val results = for {
      _ <- mangler.forbid(Set("first", "second"))
      res1 <- mangler.findAndForbidNames(Set("first", "second"))
      res2 <- mangler.findAndForbidNames(Set("first"))
      _ <- mangler.forbid(Set("first"))
      _ <- mangler.forbid(Set("first", "second"))
      _ <- mangler.forbid(Set("second"))
      res3 <- mangler.findAndForbidNames(Set("second"))
      res4 <- mangler.findAndForbidNames(Set("second", "first"))
      fn <- mangler.getForbiddenNames
    } yield (res1, res2, res3, res4, fn)

    val (r1, r2, r3, r4, _) = results.runA(ManglerState()).value
    r1 shouldBe Map("first" -> "first-0", "second" -> "second-0")
    r2 shouldBe Map("first" -> "first-1")
    r3 shouldBe Map("second" -> "second-1")
    r4 shouldBe Map("first" -> "first-2", "second" -> "second-2")
  }

}
