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

import { IFluenceClient } from "@fluencelabs/js-client";
import {
  topologyTest,
  registerTesto,
  registerLocalPrint,
  topologyBug205,
  topologyBug394,
  topologyBug427,
  topologyBug257,
} from "../compiled/examples/topology.js";

export async function topologyBug394Call(
  peer1: string,
  relay1: string,
  peer2: string,
  relay2: string,
): Promise<string> {
  return topologyBug394(relay1, peer2, relay2);
}

export async function topologyBug205Call(
  relay1: string,
  relay2: string,
): Promise<string[]> {
  return topologyBug205(relay1, relay2);
}

export async function topologyBug427Call(
  relay1: string,
  relay2: string,
): Promise<string[]> {
  return topologyBug427([relay1, relay2]);
}

export async function topologyCall(
  peer1: IFluenceClient,
  relay1: string,
  peer2: IFluenceClient,
  relay2: string,
): Promise<string> {
  const relayPeerId = relay1;
  const selfPeerId = peer1.getPeerId();

  const relayPeerId2 = relay2;
  const selfPeerId2 = peer2.getPeerId();

  registerTesto(peer2, {
    getString: (args0) => {
      console.log("hello from client2: " + args0);
      return "hello from client2: " + args0;
    },
  });

  registerLocalPrint({
    print: (args0) => {
      console.log("print on client1: " + args0);
    },
  });

  return await topologyTest(
    selfPeerId,
    relayPeerId,
    selfPeerId2,
    relayPeerId2,
    {
      ttl: 10000,
    },
  );
}

export async function topologyBug257Call(
  peer2: IFluenceClient,
): Promise<string[]> {
  return await topologyBug257(peer2.getPeerId(), peer2.getRelayPeerId());
}
