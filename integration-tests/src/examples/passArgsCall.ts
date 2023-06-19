import { Fluence } from '@fluencelabs/fluence';
import {bugLNG60, create_client_util, registerAquaDHT} from '../compiled/examples/passArgs.js';

export async function passArgsCall() {
    registerAquaDHT({
        put_host_value: (args0, args1) => {
            return args0 + args1;
        },
    });

    return await create_client_util('sid');
}

export async function bugLNG60Call(relayPeerId: string): Promise<boolean> {
    return bugLNG60(relayPeerId, {ttl: 10000})
}
