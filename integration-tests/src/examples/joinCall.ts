import {joinIdx, joinIdxLocal, joinIdxRelay} from "../compiled/examples/join.js";
import { config } from '../config.js';

const relays = config.relays

export async function joinIdxCall(relayPeerId: string) {
    // join.aqua

    return await joinIdx(1, [relayPeerId, relays[2].peerId, relays[4].peerId, relays[5].peerId], {ttl: 10000});
}

export async function joinIdxLocalCall(relayPeerId: string) {
    // join.aqua

    return await joinIdxLocal(2, [relayPeerId, relays[2].peerId, relays[4].peerId]);
}

export async function joinIdxRelayCall(relayPeerId: string) {
    // join.aqua

    return await joinIdxRelay(2, [relayPeerId, relays[2].peerId, relays[4].peerId], {ttl: 30000});
}
