import {
  test,
  testCapture,
  registerTestService,
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
    registerTestService(id, {
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
    registerTestService(id, {
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
