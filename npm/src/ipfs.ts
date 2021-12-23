import {create, globSource} from "ipfs-http-client";
import { Multiaddr, protocols } from "multiaddr";

import * as util from "util";

type UploadResult = {
  cid: string,
  size: number
}

export async function uploadFile(
    path: string,
    multiaddrResult: any,
    infoLogger: (s: string) => void,
    errorLogger: (s: string) => void
): Promise<UploadResult> {

  let rpcAddr;

  if (multiaddrResult.success) {
    rpcAddr = multiaddrResult.multiaddr;
  } else {
    errorLogger(
        "Failed to retrieve external api multiaddr"
    );
    throw multiaddrResult.error;
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

  return {
    cid: file.cid.toString(),
    size: file.size
  };
}
