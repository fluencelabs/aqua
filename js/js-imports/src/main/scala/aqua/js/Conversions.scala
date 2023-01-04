package aqua.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object Conversions {

  @js.native
  @JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v3impl/conversions.js", "ts2aqua")
  def ts2aqua(value: js.Dynamic, `type`: TypeDefinitionJs): js.Dynamic = js.native
}
