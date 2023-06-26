import { d, registerOpH } from '../compiled/examples/nestedFuncs.js';

export async function nestedFuncsCall(): Promise<string> {
    registerOpH({
        identity: (args0) => {
            return args0;
        },
    });

    return await d('some-str');
}
