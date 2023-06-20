/* eslint-disable */
// @ts-nocheck
/**
 *
 * This file is auto-generated. Do not edit manually: changes may be erased.
 * Generated by Aqua compiler: https://github.com/fluencelabs/aqua/.
 * If you find any bugs, please write an issue on GitHub: https://github.com/fluencelabs/aqua/issues
 * Aqua version: 0.11.5
 *
 */
import type { IFluenceClient as IFluenceClient$$, CallParams as CallParams$$ } from '@fluencelabs/js-client.api';
import {
    v5_callFunction as callFunction$$,
    v5_registerService as registerService$$,
} from '@fluencelabs/js-client.api';
    


// Services

export interface OpHaDef {
    array: (a: string, b: string, callParams: CallParams$$<'a' | 'b'>) => string[] | Promise<string[]>;
    identity: (a: string, callParams: CallParams$$<'a'>) => string | Promise<string>;
}
export function registerOpHa(service: OpHaDef): void;
export function registerOpHa(serviceId: string, service: OpHaDef): void;
export function registerOpHa(peer: IFluenceClient$$, service: OpHaDef): void;
export function registerOpHa(peer: IFluenceClient$$, serviceId: string, service: OpHaDef): void;
       

export function registerOpHa(...args: any) {
    registerService$$(
        args,
        {
    "defaultServiceId" : "op",
    "functions" : {
        "tag" : "labeledProduct",
        "fields" : {
            "array" : {
                "tag" : "arrow",
                "domain" : {
                    "tag" : "labeledProduct",
                    "fields" : {
                        "a" : {
                            "tag" : "scalar",
                            "name" : "string"
                        },
                        "b" : {
                            "tag" : "scalar",
                            "name" : "string"
                        }
                    }
                },
                "codomain" : {
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
                }
            },
            "identity" : {
                "tag" : "arrow",
                "domain" : {
                    "tag" : "labeledProduct",
                    "fields" : {
                        "a" : {
                            "tag" : "scalar",
                            "name" : "string"
                        }
                    }
                },
                "codomain" : {
                    "tag" : "unlabeledProduct",
                    "items" : [
                        {
                            "tag" : "scalar",
                            "name" : "string"
                        }
                    ]
                }
            }
        }
    }
}
    );
}
      
// Functions
export const doSmth_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (seq
                         (seq
                          (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                          (call %init_peer_id% ("getDataSrv" "arg") [] arg)
                         )
                         (ap arg.$.value arg_flat)
                        )
                        (call %init_peer_id% ("op" "identity") [arg_flat] a)
                       )
                       (call %init_peer_id% ("op" "array") [a "hello"] res)
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [res])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
export type DoSmthArgArg = { value: string; } 

export function doSmth(
    arg: DoSmthArgArg,
    config?: {ttl?: number}
): Promise<string[]>;

export function doSmth(
    peer: IFluenceClient$$,
    arg: DoSmthArgArg,
    config?: {ttl?: number}
): Promise<string[]>;

export function doSmth(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "doSmth",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
                "arg" : {
                    "tag" : "struct",
                    "name" : "Prod",
                    "fields" : {
                        "value" : {
                            "tag" : "scalar",
                            "name" : "string"
                        }
                    }
                }
            }
        },
        "codomain" : {
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
        doSmth_script
    )
}

/* eslint-enable */