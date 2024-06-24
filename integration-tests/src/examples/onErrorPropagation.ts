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
  registerTest,
  onPropagate,
  nestedOnPropagate,
  seqOnPropagate,
} from "../compiled/examples/onErrorPropagation.js";

export async function onPropagateCall(
  peer2: IFluenceClient,
  relay2: string,
): Promise<number> {
  registerTest(peer2, {
    fail(err, callParams) {
      return Promise.reject(err);
    },
  });

  return onPropagate(peer2.getPeerId(), relay2);
}

export async function nestedOnPropagateCall(
  peer2: IFluenceClient,
  relay2: string,
  iPeer: string,
  iRelay: string,
  friend: string,
): Promise<number> {
  registerTest(peer2, {
    fail(err, callParams) {
      return Promise.reject(err);
    },
  });

  return nestedOnPropagate(peer2.getPeerId(), relay2, iPeer, iRelay, friend);
}

export async function seqOnPropagateCall(
  peer2: IFluenceClient,
  relay2: string,
  iPeer: string,
  iRelay: string,
): Promise<number> {
  registerTest(peer2, {
    fail(err, callParams) {
      return Promise.reject(err);
    },
  });

  return seqOnPropagate(peer2.getPeerId(), relay2, iPeer, iRelay);
}
