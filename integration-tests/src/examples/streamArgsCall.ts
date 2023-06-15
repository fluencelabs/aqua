import { retrieve_records, registerTestService } from '../compiled/examples/streamArgs.js';

export async function streamArgsCall() {
    registerTestService({
        get_records: (key) => {
            return [key, key];
        },
    });

    return await retrieve_records('peer_id');
}
