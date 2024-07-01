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
  main,
  registerA,
  calc,
  calc2,
  ifCalc,
  bugLNG260,
} from "../compiled/examples/funcs.js";

export async function funcsCall() {
  registerA({
    getJ: (n) => {
      return n;
    },
  });

  let res1 = await main((c, arr) => {
    console.log(c + ": " + arr);
  });

  let res2 = await calc((c, arr) => {
    console.log(c + ": " + arr);
  });

  let res3 = await calc2((c, arr) => {
    console.log(c + ": " + arr);
  });

  let res4 = await ifCalc();

  return [res1, res2, res3, res4];
}

export async function bugLNG260Call(a: number, b: number) {
  return await bugLNG260(a, b);
}
