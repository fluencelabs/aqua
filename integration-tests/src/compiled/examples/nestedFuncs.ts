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

export interface OpHDef {
    identity: (s: string, callParams: CallParams$$<'s'>) => string | Promise<string>;
}
export function registerOpH(service: OpHDef): void;
export function registerOpH(serviceId: string, service: OpHDef): void;
export function registerOpH(peer: IFluenceClient$$, service: OpHDef): void;
export function registerOpH(peer: IFluenceClient$$, serviceId: string, service: OpHDef): void;
       

export function registerOpH(...args: any) {
    registerService$$(
        args,
        {
    "defaultServiceId" : "opa",
    "functions" : {
        "tag" : "labeledProduct",
        "fields" : {
            "identity" : {
                "tag" : "arrow",
                "domain" : {
                    "tag" : "labeledProduct",
                    "fields" : {
                        "s" : {
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
export const a_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                        (call %init_peer_id% ("getDataSrv" "b") [] b)
                       )
                       (call %init_peer_id% ("opa" "identity") [b] c)
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [c])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function a(
    b: string,
    config?: {ttl?: number}
): Promise<string>;

export function a(
    peer: IFluenceClient$$,
    b: string,
    config?: {ttl?: number}
): Promise<string>;

export function a(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "a",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
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
                    "tag" : "scalar",
                    "name" : "string"
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
        a_script
    )
}

export const d_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                        (call %init_peer_id% ("getDataSrv" "e") [] e)
                       )
                       (call %init_peer_id% ("opa" "identity") [e] c)
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [c])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function d(
    e: string,
    config?: {ttl?: number}
): Promise<string>;

export function d(
    peer: IFluenceClient$$,
    e: string,
    config?: {ttl?: number}
): Promise<string>;

export function d(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "d",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
                "e" : {
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
        d_script
    )
}

/* eslint-enable */