import { create, globSource } from "ipfs-http-client";
import { Multiaddr, protocols } from "multiaddr";
import { existsSync } from "fs";
export async function uploadFile(path, multiaddrResult, infoLogger, errorLogger) {
    let rpcAddr;
    if (multiaddrResult.success) {
        rpcAddr = multiaddrResult.multiaddr;
    }
    else {
        errorLogger("Failed to retrieve external api multiaddr");
        throw multiaddrResult.error;
    }
    let rpcMaddr = new Multiaddr(rpcAddr).decapsulateCode(protocols.names.p2p.code);
    // HACK: `as any` is needed because ipfs-http-client forgot to add `| Multiaddr` to the `create` types
    const ipfs = create(rpcMaddr);
    infoLogger("created ipfs client to " + rpcMaddr);
    await ipfs.id();
    infoLogger("connected to ipfs");
    if (!existsSync(path)) {
        let errMsg = "File does not exist: " + path;
        errorLogger(errMsg);
        throw errMsg;
    }
    const source = await globSource(path);
    const file = await ipfs.add(source);
    infoLogger("file uploaded");
    return {
        cid: file.cid.toString(),
        size: file.size
    };
}
