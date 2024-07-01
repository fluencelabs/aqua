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
  registerStringer,
  checkStreams,
  returnNilLength,
  returnEmptyLiteral,
  returnStreamFromFunc,
  streamAssignment,
  streamFunctor,
  streamIntFunctor,
  streamJoin,
  stringEmpty,
  stringNone,
} from "../compiled/examples/stream.js";

export async function streamCall() {
  registerStringer({
    returnString: (args0) => {
      return args0 + " updated";
    },
  });

  return checkStreams(["third", "fourth"]);
}

export async function returnNilCall() {
  return stringEmpty();
}

export async function returnNoneCall() {
  return stringNone();
}

export async function streamReturnFromInnerFunc() {
  return await returnStreamFromFunc();
}

export async function streamFunctorCall() {
  return await streamFunctor(["333"]);
}

export async function streamJoinCall() {
  return await streamJoin(["444"]);
}

export async function streamAssignmentCall() {
  return await streamAssignment(["333"]);
}

export async function nilLiteralCall() {
  return await returnEmptyLiteral();
}

export async function nilLengthCall() {
  return await returnNilLength();
}

export async function streamIntFunctorCall() {
  return await streamIntFunctor([0]);
}
