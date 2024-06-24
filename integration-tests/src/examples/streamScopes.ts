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
  streamIf,
  streamTry,
  streamFor,
  streamComplex,
  registerFailureSrv,
} from "../compiled/examples/streamScopes.js";

export async function streamIfCall() {
  return await streamIf();
}

export async function streamTryCall() {
  registerFailureSrv({
    fail: (msg) => {
      return Promise.reject(msg);
    },
  });

  return await streamTry();
}

export async function streamForCall() {
  return await streamFor();
}

export async function streamComplexCall() {
  registerFailureSrv({
    fail: (msg) => {
      return Promise.reject(msg);
    },
  });

  return await streamComplex();
}
