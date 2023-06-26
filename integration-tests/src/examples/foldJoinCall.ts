import { getTwoResults } from '../compiled/examples/foldJoin.js';

export async function foldJoinCall(relayPeerId: string): Promise<number[]> {
    return await getTwoResults(relayPeerId, {ttl: 16000});
}
