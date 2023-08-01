import { main, compareStreams, registerEffector } from '../compiled/examples/boolAlgebra.js';

export async function boolAlgebraCall(relay: string): Promise<boolean[]> {
    registerEffector({
        effect(name, _) {
            if (name == 'true') return Promise.resolve(true);
            else return Promise.reject(`unknown effect: ${name}`);
        },
    });

    return await main(relay);
}

export async function compareStreamsCall(relay: string): Promise<boolean> {
    return await compareStreams(relay);
}
