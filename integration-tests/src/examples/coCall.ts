import { Fluence } from '@fluencelabs/fluence';
import { parFunc } from '../compiled/examples/par.js';
import { registerCoService } from '../compiled/examples/co.js';
import {relay1} from "../__test__/examples.spec.js";

export async function coCall(): Promise<string[]> {
    registerCoService({
        call: () => {
            return 'hello';
        },
    });

    return new Promise<string[]>((resolve, reject) => {
        parFunc(relay1.peerId, (c) => {
            resolve(c.external_addresses);
        }, {ttl: 60000});
    });
}
