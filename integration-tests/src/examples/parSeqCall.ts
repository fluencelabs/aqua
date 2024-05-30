import {
  testParSeq
} from "../compiled/examples/parseq.js";
import { config } from "../config.js";

const relays = config.relays;

export async function testParSeqCall() {
    return testParSeq(relays[3].peerId, relays[4].peerId, relays[5].peerId)
}