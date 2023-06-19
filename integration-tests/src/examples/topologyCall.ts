import { IFluenceClient } from '@fluencelabs/js-client.api';
import {
    topologyTest,
    registerTesto,
    registerLocalPrint,
    topologyBug205,
    topologyBug394, topologyBug427
} from '../compiled/examples/topology.js';

export async function topologyBug394Call(peer1: string, relay1: string, peer2: string, relay2: string): Promise<string> {
    return topologyBug394(relay1, peer2, relay2)
}

export async function topologyBug205Call(relay1: string, relay2: string): Promise<string[]> {
    return topologyBug205(relay1, relay2)
}

export async function topologyBug427Call(relay1: string, relay2: string): Promise<string[]> {
    return topologyBug427([relay1, relay2])
}

export async function topologyCall(peer1: IFluenceClient, relay1: string, peer2: IFluenceClient, relay2: string): Promise<string> {
    const relayPeerId = relay1;
    const selfPeerId = peer1.getPeerId();

    const relayPeerId2 = relay2;
    const selfPeerId2 = peer2.getPeerId();

    registerTesto(peer2, {
        getString: (args0) => {
            console.log('hello from client2: ' + args0);
            return 'hello from client2: ' + args0;
        },
    });

    registerLocalPrint({
        print: (args0) => {
            console.log('print on client1: ' + args0);
        },
    });

    return await topologyTest(selfPeerId, relayPeerId, selfPeerId2, relayPeerId2, {
        ttl: 10000,
    });
}
