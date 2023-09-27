import {
  passFunctionAsArg,
  reproArgsBug426,
} from "../compiled/examples/callArrow.js";

export async function callArrowCall(relayPeerId: string): Promise<string> {
  return new Promise<string>((resolve, reject) => {
    passFunctionAsArg(
      relayPeerId,
      "callArrow call",
      (a: string) => {
        let result = "Hello, " + a + "!";
        console.log(result);
        resolve(result);
        return result;
      },
      { ttl: 10000 },
    );
  });
}

export async function reproArgsBug426Call(): Promise<string> {
  return new Promise<string>((resolve, reject) => {
    reproArgsBug426((a: string) => {
      resolve(a);
    }, "privet");
  });
}
