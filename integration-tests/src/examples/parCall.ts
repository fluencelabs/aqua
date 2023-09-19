import {parFunc, registerParService, testTimeout} from '../compiled/examples/par.js';
import {config} from "../config.js";

export async function parCall(relayPeerId: string) {
    let promise = new Promise<string>((resolve, reject) => {
        registerParService({
            call: () => {
                console.log('hello from parservice-id');
                let result = 'hello';
                resolve(result);
                return result;
            },
        });
    });

    await parFunc(relayPeerId, (c) => {
        console.log('parFunc. external addresses par: ' + c.external_addresses);
    });

    return promise;
}

const relays = config.relays

export async function testTimeoutCall() {
    return testTimeout([relays[3].peerId, relays[4].peerId])
}
