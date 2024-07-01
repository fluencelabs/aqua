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

package aqua.api

import aqua.files.Imports as IOImports

import fs2.io.file.Path

/**
 * Imports configuration passed to the compiler
 * @param settings map of path prefixes to imports settings
 */
final case class Imports(
  settings: Map[Path, Imports.PathSettings]
) {

  def toIO: IOImports =
    IOImports(
      settings.view
        .mapValues(
          _.toIO
        )
        .toMap
    )
}

object Imports {

  /**
   * Imports settings for a single path prefix.
   * @param imports map of import prefixes to locations
   */
  final case class PathSettings(
    imports: Map[String, List[Path]]
  ) {

    def toIO: IOImports.PathSettings =
      IOImports.PathSettings(imports)
  }

  def fromMap(m: Map[String, Map[String, List[String]]]): Imports =
    Imports(
      m.map { case (pathPrefix, settings) =>
        Path(pathPrefix) -> PathSettings(
          settings.map { case (importPrefix, locations) =>
            importPrefix -> locations.map(Path.apply)
          }
        )
      }
    )
}
