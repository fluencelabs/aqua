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
  compareStreams,
  compareStructs,
  registerEffector,
} from "../compiled/examples/boolAlgebra.js";

export async function boolAlgebraCall(relay: string): Promise<boolean[]> {
  registerEffector({
    effect(name, _) {
      if (name == "true") return Promise.resolve(true);
      else return Promise.reject(`unknown effect: ${name}`);
    },
  });

  return await main(relay);
}

export async function compareStreamsCall(relay: string): Promise<boolean> {
  return await compareStreams(relay);
}

export async function compareStructsCall(
  relay: string,
  str: string,
): Promise<boolean> {
  return await compareStructs(relay, str);
}
