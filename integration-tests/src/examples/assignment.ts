import { doSmth } from '../compiled/examples/assignment.js';

export async function assignmentCall(): Promise<string[]> {
    return await doSmth({ value: 'abc' }, { ttl: 6000 });
}
