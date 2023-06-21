/* eslint-disable */
// @ts-nocheck
/**
 *
 * This file is auto-generated. Do not edit manually: changes may be erased.
 * Generated by Aqua compiler: https://github.com/fluencelabs/aqua/.
 * If you find any bugs, please write an issue on GitHub: https://github.com/fluencelabs/aqua/issues
 * Aqua version: 0.11.7
 *
 */
import type { IFluenceClient as IFluenceClient$$, CallParams as CallParams$$ } from '@fluencelabs/js-client.api';
import {
    v5_callFunction as callFunction$$,
    v5_registerService as registerService$$,
} from '@fluencelabs/js-client.api';
    


// Services

// Functions
export const someFunc_script = `
                    (xor
                     (seq
                      (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                      (new $ifaces
                       (xor
                        (seq
                         (canon %init_peer_id% $ifaces  #ifaces_canon)
                         (call %init_peer_id% ("callbackSrv" "cb") [#ifaces_canon])
                        )
                        (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                       )
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function someFunc(
    cb: (arg0: string[], callParams: CallParams$$<'arg0'>) => void | Promise<void>,
    config?: {ttl?: number}
): Promise<void>;

export function someFunc(
    peer: IFluenceClient$$,
    cb: (arg0: string[], callParams: CallParams$$<'arg0'>) => void | Promise<void>,
    config?: {ttl?: number}
): Promise<void>;

export function someFunc(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "someFunc",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
                "cb" : {
                    "tag" : "arrow",
                    "domain" : {
                        "tag" : "unlabeledProduct",
                        "items" : [
                            {
                                "tag" : "array",
                                "type" : {
                                    "tag" : "scalar",
                                    "name" : "string"
                                }
                            }
                        ]
                    },
                    "codomain" : {
                        "tag" : "nil"
                    }
                }
            }
        },
        "codomain" : {
            "tag" : "nil"
        }
    },
    "names" : {
        "relay" : "-relay-",
        "getDataSrv" : "getDataSrv",
        "callbackSrv" : "callbackSrv",
        "responseSrv" : "callbackSrv",
        "responseFnName" : "response",
        "errorHandlingSrv" : "errorHandlingSrv",
        "errorFnName" : "error"
    }
},
        someFunc_script
    )
}

/* eslint-enable */