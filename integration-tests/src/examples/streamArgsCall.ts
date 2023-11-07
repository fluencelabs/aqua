import {
  retrieve_records,
  registerTestService,
  lng280Bug
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
