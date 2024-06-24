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
  getObj,
  getObjAssign,
  getObjRelay,
  getObjFor,
} from "../compiled/examples/object.js";

export async function getObjCall() {
  return await getObj();
}

export async function getObjRelayCall() {
  return await getObjRelay();
}

export async function getObjAssignCall() {
  return await getObjAssign();
}

export async function getObjForCall() {
  return await getObjFor();
}
