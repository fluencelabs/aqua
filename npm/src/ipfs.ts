import {create, CID, globSource} from "ipfs-http-client";
import { Multiaddr, protocols } from "multiaddr";

import { FluencePeer } from "@fluencelabs/fluence";
import {
  get_external_api_multiaddr,
  get_external_swarm_multiaddr,
} from "./compiled/ipfs.js";
import { AddResult } from "ipfs-core-types/src/root";
import * as util from "util";

export async function uploadFile(
    path: string,
    provider: FluencePeer,
    infoLogger: (s: string) => void,
    errorLogger: (s: string) => void
): Promise<string> {
  const relayPeerId = provider.getStatus().relayPeerId!;

  let rpcAddr;
  let result = await get_external_api_multiaddr(provider, relayPeerId);
  if (result.success) {
    rpcAddr = result.multiaddr;
  } else {
    errorLogger(
        "Failed to retrieve external api multiaddr from " +
        relayPeerId
    );
    throw result.error;
  }

  let rpcMaddr = new Multiaddr(rpcAddr).decapsulateCode(
      protocols.names.p2p.code
  );
  // HACK: `as any` is needed because ipfs-http-client forgot to add `| Multiaddr` to the `create` types
  const ipfs = create(rpcMaddr as any);
  infoLogger("created ipfs client to " + rpcMaddr);

  await ipfs.id();
  infoLogger("connected to ipfs");

  const source: any = await globSource(path)
  const file = await ipfs.add(source);

  const msg = util.format("uploaded file: %s", file)
  infoLogger(msg);

  return file.cid.toString();
}
