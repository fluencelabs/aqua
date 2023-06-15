import {
    arraySugar,
    bugLNG59,
    optionSugar,
    registerGetArr,
    streamSugar
} from "../compiled/examples/collectionSugar.js";

export async function arraySugarCall(): Promise<[number[], number[]]> {
    return await arraySugar(3, 6)
}

export async function streamSugarCall(): Promise<[number[], number[]]> {
    return await streamSugar(3, 6)
}

export async function optionSugarCall(): Promise<[number[], string[], string[]]> {
    return await optionSugar(1, "some", null, null)
}

export async function bugLNG59Call(nodes: string[]): Promise<string> {

    registerGetArr({
        getArr: () => {
            return nodes
        }
    })

    const a = await bugLNG59()
    return a
}