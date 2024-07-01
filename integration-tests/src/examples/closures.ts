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
  closureIn,
  closureOut,
  closureBig,
  registerLocalSrv,
  closureOut2,
  lng58Bug,
  lng317Bug,
  multipleClosuresBugLNG262,
  lng325Bug,
  lng325BugTwoFuncs
} from "../compiled/examples/closures.js";
import { config } from "../config.js";

const relays = config.relays;

export async function closuresCall(): Promise<
  [string, string[], string[], [string, string]]
> {
  registerLocalSrv({ inside: () => console.log("call inside") });

  console.log("closurein");
  const resIn = await closureIn(relays[4].peerId, { ttl: 25000 });
  console.log("closureout");
  const resOut = await closureOut(relays[5].peerId, { ttl: 25000 });
  console.log("closureout2");
  const resOut2 = await closureOut2(relays[5].peerId, { ttl: 25000 });
  console.log("closurebig");
  const resBig = await closureBig(relays[4].peerId, relays[5].peerId, {
    ttl: 25000,
  });

  return [resIn, resOut.external_addresses, resOut2.external_addresses, resBig];
}

export async function lng58CBugCall(): Promise<string> {
  return lng58Bug();
}

export async function multipleClosuresLNG262BugCall(): Promise<[number, number]> {
  return multipleClosuresBugLNG262();
}

export async function lng317BugCall(): Promise<string[]> {
  return lng317Bug();
}

export async function lng325BugCall(): Promise<string> {
  return lng325Bug();
}

export async function lng325BugTwoFuncsCall(): Promise<[string, string]> {
  return lng325BugTwoFuncs();
}
