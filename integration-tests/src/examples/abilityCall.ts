import {handleAb, registerSomeService} from "../compiled/examples/abilities";

export async function abilityCall(): Promise<[string, string, string, number]> {
    registerSomeService({
        getStr: (s: string) => {
            return s + "123"
        }
    })

    return await handleAb("some_string")
}
