import {bugLng79, registerSer} from "../compiled/examples/canon.js";

export async function bugLng79Call(pid: string, relay: string): Promise<number> {
    registerSer({
        getRecord: () => {
            return {peer_id: pid, relay_id: [relay]};
        }
    })
    return await bugLng79((s) => {
        console.log(s)
    });
}
