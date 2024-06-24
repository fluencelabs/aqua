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
  test,
  registerTest,
  TestResult,
} from "../compiled/examples/nestedData.js";

export async function nestedDataCall(): Promise<TestResult> {
  let nested = {
    one: {
      val: "hello",
    },
  };
  registerTest({
    test1: () => {
      return nested;
    },
    test2: (arg1: { val: string }, arg2: string) => {
      let res = {
        one: {
          val: arg1.val + arg2,
        },
      };
      return res;
    },
  });

  return await test();
}
