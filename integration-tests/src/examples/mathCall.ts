import {test1, test2, testI16, testI32, testI64, testU64} from '../compiled/examples/math.js';

export async function mathTest1Call(): Promise<number> {
    return await test1();
}

export async function mathTest2Call(): Promise<number> {
    return await test2();
}

export async function mathTestI16Call(peer: string): Promise<number[]> {
    return await testI16(peer);
}

export async function mathTestI32Call(peer: string): Promise<number[]> {
    return await testI32(peer);
}

export async function mathTestI64Call(peer: string): Promise<number[]> {
    return await testI64(peer);
}

export async function mathTestU64Call(peer: string): Promise<number[]> {
    return await testU64(peer);
}