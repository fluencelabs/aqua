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

import {
  concat_foobars,
  registerStringService,
} from "../compiled/examples/imports_exports/imports.js";
import { registerMyExportSrv } from "../compiled/examples/imports_exports/exports.js";
import { registerSuperFoo } from "../compiled/examples/imports_exports/declare.js";

export async function declareCall() {
  registerSuperFoo({
    small_foo: () => {
      return "small_foo";
    },
  });

  registerStringService({
    concat: (a, b) => {
      return a + b;
    },
  });
  registerMyExportSrv({
    another_str: () => {
      return "str_from_my_export_srv";
    },
  });
  return await concat_foobars();
}
