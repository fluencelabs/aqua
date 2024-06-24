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
  checkEmpty,
  checkNoneEmpty,
  emptyString,
  registerOptionString,
} from "../compiled/examples/options/option_gen.js";

export async function genOptions(): Promise<[string, string]> {
  registerOptionString({
    checkOption: (str: string | null) => {
      if (str) {
        return "some";
      } else {
        return "none";
      }
    },
  });

  const a = await checkEmpty();
  const b = await checkNoneEmpty("some_string");
  return [a, b];
}

export async function genOptionsEmptyString(): Promise<string | null> {
  return await emptyString();
}
