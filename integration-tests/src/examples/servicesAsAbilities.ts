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
  test,
  testCapture,
  registerTestServiceAsAbility,
} from "../compiled/examples/servicesAsAbilities.js";

const serviceIds = {
  default: "default-id",
  resolved1: "resolved-id-1",
  resolved2: "resolved-id-2",
  iter1: "iter-id-1",
  iter2: "iter-id-2",
};

const serviceIdsSequence = [
  serviceIds.default,
  serviceIds.default,
  serviceIds.resolved1,
  serviceIds.resolved1,
  serviceIds.default,
  serviceIds.iter1,
  serviceIds.iter2,
  serviceIds.resolved1,
  serviceIds.resolved2,
];

const msgs = [
  "call",
  "capture call",
  "accept closure call",
  "accept ability call",
];

export const expectedServiceResults = serviceIdsSequence.flatMap((id) =>
  msgs.map((msg) => `${id}: ${msg}`),
);

export async function servicesAsAbilitiesCall() {
  Object.entries(serviceIds).forEach(([_key, id], _idx) =>
    registerTestServiceAsAbility(id, {
      concatId: (s: string) => {
        return `${id}: ${s}`;
      },
      getId: () => {
        return id;
      },
    }),
  );

  return await test();
}

export const expectedServiceCaptureResults = [
  "resolved-id-in-capture: in capture",
  "default-id: in capture",
];

export async function servicesAsAbilitiesCaptureCall() {
  ["resolved-id-in-capture", "default-id"].forEach((id) =>
    registerTestServiceAsAbility(id, {
      concatId: (s: string) => {
        return `${id}: ${s}`;
      },
      getId: () => {
        return id;
      },
    }),
  );

  return await testCapture();
}
