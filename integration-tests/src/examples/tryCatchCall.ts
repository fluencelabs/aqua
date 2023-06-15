import { Fluence } from '@fluencelabs/fluence';
import { tryCatchTest } from '../compiled/examples/tryCatch.js';

export async function tryCatchCall(relayPeerId: string): Promise<string[]> {
    return await tryCatchTest(relayPeerId, {ttl: 60000});
}
