import { rename_s } from '../compiled/examples/renameVars.js';

export async function renameVarsCall(): Promise<string[]> {
    return await rename_s();
}
