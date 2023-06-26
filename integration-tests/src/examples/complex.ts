import { doStuff, registerTestS } from '../compiled/examples/complex.js';

export async function complexCall(selfPeerId: string, relayPeerId: string) {

    registerTestS({
        t: (arg0) => {
            return arg0;
        },
        multiline: (a, b, c) => {
            return b;
        },
    });

    return await doStuff(relayPeerId, selfPeerId, true, true, ['1', '2'], ['3', '4'], 'some str');
}
