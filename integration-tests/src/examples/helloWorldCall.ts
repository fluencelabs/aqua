import { helloWorld, registerStringExtra } from '../compiled/examples/helloWorld.js';

export async function helloWorldCall() {
    // helloWorld.aqua
    registerStringExtra({
        addNameToHello: (args0) => {
            return `Hello, ${args0}!`;
        },
    });

    return await helloWorld('NAME');
}
