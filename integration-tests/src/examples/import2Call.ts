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

import { registerOneMore } from "../compiled/examples/imports_exports/gen/OneMore.js";
import { barfoo, wrap } from "../compiled/examples/imports_exports/import2.js";

export async function import2Call() {
  registerOneMore("hello", {
    more_call: () => {},
  });

  registerOneMore("ohmygod", {
    more_call: () => {},
  });

  let first = await wrap();
  let second = await barfoo();

  return { first, second };
}
