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

import { registerSubService } from "../compiled/examples/imports_exports/subImport.js";
import {
  registerConcatSubs,
  subImportUsage,
} from "../compiled/examples/subImportUsage.js";

export async function subImportCall() {
  // helloWorld.aqua
  registerSubService({
    sub: (s) => {
      return {
        one: s,
        two: 42,
      };
    },
  });
  registerConcatSubs({
    get_some: (s, sr) => {
      return {
        one: s,
        two: sr.two,
      };
    },
  });

  return await subImportUsage("random_string");
}
