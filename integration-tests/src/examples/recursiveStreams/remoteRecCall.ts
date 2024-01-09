import { IFluenceClient } from "@fluencelabs/js-client";
import { remoteRecStream } from "../../compiled/examples/recursiveStreams/remoteRec.js";

export async function remoteRecStreamCall(
  init: number,
  target: number,
  peer: IFluenceClient,
): Promise<number[]> {
  return await remoteRecStream(
    init,
    target,
    peer.getPeerId(),
    peer.getRelayPeerId(),
  );
}
