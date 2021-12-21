import {create, CID, globSource} from "ipfs-http-client";
import { Multiaddr, protocols } from "multiaddr";

import { FluencePeer } from "@fluencelabs/fluence";
import {
  get_external_api_multiaddr,
  get_external_swarm_multiaddr,
} from "../compiled/ipfs";
import { AddResult } from "ipfs-core-types/src/root";
const all = require('it-all')

export async function uploadFile(
    path: string,
    provider: FluencePeer
): Promise<string> {
  const relayPeerId = provider.getStatus().relayPeerId!;

  let rpcAddr;
  let result = await get_external_api_multiaddr(provider, relayPeerId);
  if (result.success) {
    rpcAddr = result.multiaddr;
  } else {
    console.error(
        "Failed to retrieve external api multiaddr from %s: ",
        relayPeerId
    );
    throw result.error;
  }

  let rpcMaddr = new Multiaddr(rpcAddr).decapsulateCode(
      protocols.names.p2p.code
  );
  // HACK: `as any` is needed because ipfs-http-client forgot to add `| Multiaddr` to the `create` types
  const ipfs = create(rpcMaddr as any);
  console.log("📗 created ipfs client to %s", rpcMaddr);

  await ipfs.id();
  console.log("📗 connected to ipfs");

  const source: any = await globSource(path)
  const file = await ipfs.add(source);
  console.log("📗 uploaded file:", file);

  return file.cid.toString();
}
