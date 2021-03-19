import { FluenceClient, PeerIdB58 } from '@fluencelabs/fluence';
import { RequestFlowBuilder } from '@fluencelabs/fluence/dist/api.unstable';
export async function getTime(client: FluenceClient, peerId: PeerIdB58, ret: (time: number) => void): Promise<string> {
    let request;
    const promise = new Promise<string>((resolve, reject) => {
        request = new RequestFlowBuilder()
            .withRawScript(
                `
            text of the generated script
            on many
            lines
            `,
            )
            .configHandler((h) => {
                h.on('getRelayService', 'getRelay', () => {
                    return client.relayPeerId;
                });
                h.on('getRelayService', 'hasReleay', () => {
                    return client.relayPeerId !== undefined;
                });
                h.on('nameForParamsGetterService', 'getPeerID', () => {
                    return peerId;
                });
                h.on('nameForServiceIdGenertedForPlaceWhereRetIsCalled', 'retProbably', (args) => {
                    (ret as any).apply(args);
                });
                h.on('nameForServiceWhichResolvesPromise', 'callbackOrAnythingReally', (args) => {
                    // args is an array of all the arguments to function.
                    // Extract the right one from the args. If there is only 1 item, you can always use
                    // the costruct below
                    const [res] = args;
                    resolve(res);
                });
                h.on('nameOfServiceWhereToSendXorError', 'errorProbably', (args) => {
                    // assuming error is the single argument
                    const [err] = args;
                    reject(err);
                });
            })
            .handleTimeout(() => {
                reject('message for timeout');
            })
            .build();
    });
    await client.initiateFlow(request);
    return promise;
}