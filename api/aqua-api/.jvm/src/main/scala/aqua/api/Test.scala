package aqua.api
import cats.effect.{IO, IOApp}
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend

object Test extends IOApp.Simple {
  override def run: IO[Unit] = {
    val input =
      """func getNumber(number: u32) -> u32:
        |    <- number
        |""".stripMargin
    APICompilation.compileString(input, Nil, AquaAPIConfig(targetType = JavaScriptType), JavaScriptBackend()).map {
      res =>
        println(res)
    }
  }
}
