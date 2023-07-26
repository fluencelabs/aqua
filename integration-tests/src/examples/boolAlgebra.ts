import {main, registerEffector} from "../compiled/examples/boolAlgebra.js";

export async function boolAlgebraCall(relay: string): Promise<boolean[]> {
    registerEffector({
        effect(name, _) {
            console.log(`effect: ${name}`);
            if (name == "true") return Promise.resolve(true);
            else return Promise.reject(`unknown effect: ${name}`)
        },
    });

    return await main(relay);
}