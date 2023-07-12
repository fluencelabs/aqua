import {IFluenceClient} from '@fluencelabs/js-client.api';
import {registerTest, onPropagate, nestedOnPropagate, seqOnPropagate} from "../compiled/examples/onErrorPropagation.js"

export async function onPropagateCall(peer2: IFluenceClient, relay2: string): Promise<number> {    
    registerTest(peer2, {
        fail(err, callParams) {
            return Promise.reject(err);
        },
    })

    return onPropagate(peer2.getPeerId(), relay2)
}

export async function nestedOnPropagateCall(
    peer2: IFluenceClient, 
    relay2: string,
    iPeer: string,
    iRelay: string,
    friend: string
): Promise<number> {    
    registerTest(peer2, {
        fail(err, callParams) {
            return Promise.reject(err);
        },
    })

    return nestedOnPropagate(peer2.getPeerId(), relay2, iPeer, iRelay, friend)
}

export async function seqOnPropagateCall(
    peer2: IFluenceClient, 
    relay2: string,
    iPeer: string,
    iRelay: string
): Promise<number> {    
    registerTest(peer2, {
        fail(err, callParams) {
            return Promise.reject(err);
        },
    })

    return seqOnPropagate(peer2.getPeerId(), relay2, iPeer, iRelay)
}