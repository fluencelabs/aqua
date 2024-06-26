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
  handleAb,
  registerSomeService,
  bug214,
  checkAbCalls,
  bugLNG258_1,
  bugLNG258_2,
  bugLNG258_3,
  multipleAbilityWithClosure,
  registerMySrv,
  returnSrvAsAbility,
} from "../compiled/examples/abilities.js";

export async function abilityCall(): Promise<[string, string, string, number]> {
  registerSomeService({
    getStr: (s: string) => {
      return s + "123";
    },
  });

  return await handleAb("some_string");
}

export async function complexAbilityCall(): Promise<[boolean, boolean]> {
  return await bug214();
}

export async function checkAbCallsCall(): Promise<[boolean, boolean, boolean]> {
  return await checkAbCalls();
}

export async function bugLNG258Call1(): Promise<[number, number]> {
  return await bugLNG258_1();
}

export async function bugLNG258Call2(): Promise<[number, number]> {
  return await bugLNG258_2();
}

export async function bugLNG258Call3(): Promise<[number, number]> {
  return await bugLNG258_3();
}

export async function multipleAbilityWithClosureCall(): Promise<
  [number, number]
> {
  return await multipleAbilityWithClosure();
}

export async function returnSrvAsAbilityCall(): Promise<string[]> {
  const srv = (id: string) => {
    return {
      call: () => {
        return id;
      },
    };
  };

  registerMySrv("default-id", srv("default-id"));
  registerMySrv("resolved-id", srv("resolved-id"));

  return await returnSrvAsAbility();
}
