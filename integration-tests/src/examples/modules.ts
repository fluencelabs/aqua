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
  renamed, merged, exported
} from "../compiled/examples/imports_exports/modules/main.js";

export async function renamedCall(): Promise<number> {
  return await renamed();
}

export async function mergedCall(): Promise<number> {
  return await merged();
}

export async function exportedCall(): Promise<number> {
  return await exported();
}