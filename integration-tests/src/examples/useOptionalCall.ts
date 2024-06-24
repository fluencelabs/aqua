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
  returnNone,
  returnOptional,
  useOptional,
  registerSomeS,
  getDefault,
  getArg
} from "../compiled/examples/option.js";

export function registerHandlers(): void {
  registerSomeS({
    getStr: (arg0) => {
      return arg0;
    },
    getStr1: () => {
      return "optional";
    },
    getStr2: (arg0) => {
      return arg0;
    },
    checkU32: (arg) => {},
  });
}

export async function useOptionalCall(): Promise<string> {
  return await useOptional("hello");
}

export async function returnOptionalCall(): Promise<string | null> {
  return await returnOptional();
}

export async function returnNull(): Promise<string | null> {
  return await returnNone();
}

export async function getDefaultCall(): Promise<number> {
  return await getDefault();
}

export async function getArgCall(num: number): Promise<number> {
  return await getArg(num);
}
