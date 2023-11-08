import {
  retrieve_records,
  registerTestService,
  registerStreamService,
  lng280Bug,
  lng280BugWithFor,
  lng280BugWithForAnonStream,
} from "../compiled/examples/streamArgs.js";

export async function streamArgsCall() {
  registerTestService({
    get_records: (key) => {
      return [key, key];
    },
  });

  return await retrieve_records("peer_id");
}

export async function lng280BugCall(): Promise<string[]> {
  return lng280Bug()
}

export async function lng280BugWithForCall(): Promise<string[]> {
  return lng280BugWithFor()
}

export async function lng280BugWithForAnonStreamCall(): Promise<number[][]> {
  let storage: number[][] = []
  registerStreamService({
    store: (numbers, n) => {
      numbers.push(n)
      storage.push(numbers)
    },
  });
  await lng280BugWithForAnonStream()

  return storage
}
