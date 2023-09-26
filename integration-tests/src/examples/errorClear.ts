import { IFluenceClient } from "@fluencelabs/js-client";
import {
  errorClearTest,
  registerFailService,
} from "../compiled/examples/errorClear.js";

export async function errorClearCall(peer: IFluenceClient) {
  registerFailService(peer, {
    call: (s: string) => {
      if (s === "fail") return Promise.reject(s);
      else return Promise.resolve(s);
    },
  });

  return await errorClearTest(peer.getPeerId(), peer.getRelayPeerId());
}
