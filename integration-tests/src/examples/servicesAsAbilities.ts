import { test, registerTestService } from '../compiled/examples/servicesAsAbilities.js';

export const serviceIds = ['default-id', 'resolved-id-1', 'resolved-id-2', 'iter-id-1', 'iter-id-2'];

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
