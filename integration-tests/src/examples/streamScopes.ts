import {
    streamIf,
    streamTry,
    streamFor,
    streamComplex,
    registerFailureSrv,
} from '../compiled/examples/streamScopes.js';

export async function streamIfCall() {
    return await streamIf();
}

export async function streamTryCall() {
    registerFailureSrv({
        fail: (msg) => {
            return Promise.reject(msg);
        },
    });

    return await streamTry();
}

export async function streamForCall() {
    return await streamFor();
}

export async function streamComplexCall() {
    registerFailureSrv({
        fail: (msg) => {
            return Promise.reject(msg);
        },
    });

    return await streamComplex();
}
