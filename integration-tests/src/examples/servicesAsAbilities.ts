import { test, registerTestService } from '../compiled/examples/servicesAsAbilities.js';

const serviceIds = ['default-id', 'resolved-id-1', 'resolved-id-2', 'iter-id-1', 'iter-id-2'];

const serviceIdsSequence = [
    serviceIds[0], // dafault
    serviceIds[0], // default
    serviceIds[1], // resolved 1
    serviceIds[1], // resolved 1
    serviceIds[0], // default
    serviceIds[3], // iter 1
    serviceIds[4], // iter 2
    serviceIds[1], // resolved 1
    serviceIds[2], // resolved 2
];

const msgs = ['call', 'capture call', 'accept closure call', 'accept ability call'];

export const expectedServiceResults = serviceIdsSequence.flatMap((id) => msgs.map((msg) => `${id}: ${msg}`));

export async function servicesAsAbilitiesCall() {
    serviceIds.forEach((id) =>
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
