import { streamRes } from '../compiled/examples/streamRestriction.js';

export async function streamResCall(): Promise<any> {
    return await streamRes(["a", "b", "c"]);
}
