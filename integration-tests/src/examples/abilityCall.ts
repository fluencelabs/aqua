import {handleAb, registerSomeService, bug214} from "../compiled/examples/abilities";

export async function abilityCall(): Promise<[string, string, string, number]> {
    registerSomeService({
        getStr: (s: string) => {
            return s + "123"
        }
    })

    return await handleAb("some_string")
}

export async function complexAbilityCall(): Promise<[boolean, boolean]> {
    return await bug214()
}
