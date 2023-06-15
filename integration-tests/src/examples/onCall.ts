import { Fluence } from '@fluencelabs/fluence';
import { getPeerExternalAddresses } from '../compiled/examples/on.js';

export async function onCall(relayPeerId: string): Promise<string[]> {
    return await getPeerExternalAddresses(relayPeerId);
}
