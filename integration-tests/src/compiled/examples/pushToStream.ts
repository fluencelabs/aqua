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

export interface OpADef {
    get_str: (callParams: CallParams$$<null>) => string | Promise<string>;
}
export function registerOpA(service: OpADef): void;
export function registerOpA(serviceId: string, service: OpADef): void;
export function registerOpA(peer: IFluenceClient$$, service: OpADef): void;
export function registerOpA(peer: IFluenceClient$$, serviceId: string, service: OpADef): void;
       

export function registerOpA(...args: any) {
    registerService$$(
        args,
        {
    "defaultServiceId" : "pop",
    "functions" : {
        "tag" : "labeledProduct",
        "fields" : {
            "get_str" : {
                "tag" : "arrow",
                "domain" : {
                    "tag" : "nil"
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
export const get_results_script = `
                    (xor
                     (seq
                      (seq
                       (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                       (new $results
                        (seq
                         (seq
                          (seq
                           (seq
                            (ap "hello" $results)
                            (call %init_peer_id% ("pop" "get_str") [] str)
                           )
                           (ap str $results)
                          )
                          (canon %init_peer_id% $results  #-results-fix-0)
                         )
                         (ap #-results-fix-0 -results-flat-0)
                        )
                       )
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [-results-flat-0])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function get_results(
    config?: {ttl?: number}
): Promise<string[]>;

export function get_results(
    peer: IFluenceClient$$,
    config?: {ttl?: number}
): Promise<string[]>;

export function get_results(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "get_results",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
                
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
        get_results_script
    )
}

/* eslint-enable */