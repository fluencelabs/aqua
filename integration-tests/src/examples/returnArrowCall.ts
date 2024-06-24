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
  callReturnedArrow,
  callReturnedChainArrow,
} from "../compiled/examples/returnArrow.js";

export async function returnArrowCall(): Promise<[string, string]> {
  return await callReturnedArrow("arg for func ", "arg for closure ");
}

export async function returnArrowChainCall(): Promise<
  [string, string, string, string, string, string, string, string]
> {
  return await callReturnedChainArrow("arg for func1 ", "arg for func2 ");
}
