/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.js

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSImport}

object Meta {

  // get `import`.meta.url info from javascript
  // it is needed for `createRequire` function
  // TODO: Investigate if it is really needed
  @js.native
  @JSImport("./meta-utils.js", "metaUrl")
  val metaUrl: String = js.native
}

// Require function from javascript
trait Require extends js.Object {

  // resolve path to module
  def resolve(request: String): String
}

@js.native
@JSImport("module", JSImport.Namespace)
object Module extends js.Object {

  // make it possible to use `require` in ES module type
  def createRequire(filename: String): Require = js.native
}

object Npm {
  private def require = Module.createRequire(Meta.metaUrl)

  // Resolve path to module
  def resolveModule(path: String): String = require.resolve(path)
}
