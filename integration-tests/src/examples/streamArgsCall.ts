import {
  retrieve_records,
  modify_stream,
  registerTestService,
} from "../compiled/examples/streamArgs.js";

export async function streamArgsCall() {
  registerTestService({
    get_records: (key) => {
      return [key, key];
    },
  });

  return await retrieve_records("peer_id");
}

export async function modifyStreamCall(arg: string[]) {
  return await modify_stream(arg);
}
