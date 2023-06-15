import {checkEmpty, checkNoneEmpty, emptyString, registerOptionString} from "../compiled/examples/options/option_gen.js";

export async function genOptions(): Promise<[string, string]> {
    registerOptionString({
        checkOption: (str: string | null) => {
            if (str) {
                return "some"
            } else {
                return "none"
            }
        }
    })

    const a = await checkEmpty();
    const b = await checkNoneEmpty("some_string");
    return [a, b]
}

export async function genOptionsEmptyString(): Promise<string | null> {
    return await emptyString();
}