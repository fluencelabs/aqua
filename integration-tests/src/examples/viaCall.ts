import { viaArr, viaOpt, viaStream } from '../compiled/examples/via.js';
import { config } from '../config.js';

const relays = config.relays

export async function viaArrCall(): Promise<string[]> {
    let res = await viaArr(relays[4].peerId, [relays[2].peerId, relays[1].peerId], {ttl: 30000});

    return res.external_addresses;
}

export async function viaOptCall(relayPeerId: string): Promise<string[]> {

    let res2 = await viaOpt(relayPeerId, relays[4].peerId, relays[2].peerId, {ttl: 30000});

    return res2.external_addresses;
}

export async function viaOptNullCall(relayPeerId: string): Promise<string[]> {

    let res3 = await viaOpt(relayPeerId, relays[4].peerId, relays[2].peerId || null, {ttl: 30000});

    return res3.external_addresses;
}

export async function viaStreamCall(relayPeerId: string): Promise<string[]> {

    let res4 = await viaStream(relays[4].peerId, [relays[2].peerId, relays[1].peerId], {ttl: 30000});

    return res4.external_addresses;
}
