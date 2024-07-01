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

import { lng193Bug, lng365Bug } from "../compiled/examples/closureReturnRename.js";
import { config } from "../config.js";

const relays = config.relays;

export async function lng193BugCall(): Promise<number> {
  return lng193Bug(relays[4].peerId, relays[5].peerId);
}

export async function lng365BugCall() {
  return lng365Bug();
}
