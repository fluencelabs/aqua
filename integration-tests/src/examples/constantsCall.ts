import {callConstant, registerGetter, timestampAndTtl} from '../compiled/examples/constants.js';

export async function constantsCall(): Promise<string[]> {
    registerGetter({
        createStr: (arg0) => {
            return '' + arg0;
        },
    });

    return await callConstant();
}

export async function particleTtlAndTimestampCall(ttl: number): Promise<[number, number]> {

    return await timestampAndTtl({ttl: ttl});
}
