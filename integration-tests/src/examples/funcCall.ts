import { testFunc, registerTestSrv } from '../compiled/examples/func.js';

export async function funcCall() {
    registerTestSrv({
        str: () => {
            return `some str`;
        },
    });

    return await testFunc();
}
