import { tryOtherwiseTest } from '../compiled/examples/tryOtherwise.js';

export async function tryOtherwiseCall(relayPeerId: string): Promise<string> {
    return await tryOtherwiseTest(relayPeerId, {ttl: 60000});
}
