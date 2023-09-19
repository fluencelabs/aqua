package aqua.linker

import cats.data.Validated
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LinkerSpec extends AnyFlatSpec with Matchers {

  "linker" should "resolve dependencies" in {

    val empty = Modules[String, String, String => String]()

    val withMod1 =
      empty
        .add(
          AquaModule[String, String, String => String](
            id = "mod1",
            imports = Map.empty,
            dependsOn = Map("mod2" -> "unresolved mod2 in mod1"),
            body = _ ++ " | mod1"
          ),
          toExport = true
        )
    withMod1.isResolved should be(false)

    Linker.link[String, String, String](
      withMod1,
      cycle => cycle.map(_.id).toChain.toList.mkString(" -> "),
      _ => ""
    ) should be(Validated.invalidNec("unresolved mod2 in mod1"))

    val withMod2 =
      withMod1.add(AquaModule("mod2", Map.empty, Map.empty, _ ++ " | mod2"))

    withMod2.isResolved should be(true)

    Linker.link[String, String, String](
      withMod2,
      cycle => cycle.map(_.id + "?").toChain.toList.mkString(" -> "),
      _ => ""
    ) should be(Validated.validNec(Map("mod1" -> " | mod2 | mod1")))
  }

}
