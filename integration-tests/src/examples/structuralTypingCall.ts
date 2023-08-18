import {structuralTypingTest} from "../compiled/examples/structuraltyping";

export async function structuralTypingCall(): Promise<string> {
    return await structuralTypingTest();
}
