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

export interface DTGetterDef {
    get_dt: (s: string, callParams: CallParams$$<'s'>) => { field: string; } | Promise<{ field: string; }>;
}
export function registerDTGetter(service: DTGetterDef): void;
export function registerDTGetter(serviceId: string, service: DTGetterDef): void;
export function registerDTGetter(peer: IFluenceClient$$, service: DTGetterDef): void;
export function registerDTGetter(peer: IFluenceClient$$, serviceId: string, service: DTGetterDef): void;
       

export function registerDTGetter(...args: any) {
    registerService$$(
        args,
        {
    "defaultServiceId" : "get-dt",
    "functions" : {
        "tag" : "labeledProduct",
        "fields" : {
            "get_dt" : {
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
                            "tag" : "struct",
                            "name" : "DT",
                            "fields" : {
                                "field" : {
                                    "tag" : "scalar",
                                    "name" : "string"
                                }
                            }
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
export const use_name1_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (seq
                         (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                         (call %init_peer_id% ("getDataSrv" "name") [] name)
                        )
                        (call %init_peer_id% ("get-dt" "get_dt") [name] results)
                       )
                       (ap results.$.field results_flat)
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [results_flat])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function use_name1(
    name: string,
    config?: {ttl?: number}
): Promise<string>;

export function use_name1(
    peer: IFluenceClient$$,
    name: string,
    config?: {ttl?: number}
): Promise<string>;

export function use_name1(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "use_name1",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
                "name" : {
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
        use_name1_script
    )
}

export const use_name2_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                        (call %init_peer_id% ("getDataSrv" "name") [] name)
                       )
                       (new $results
                        (seq
                         (seq
                          (seq
                           (seq
                            (seq
                             (seq
                              (seq
                               (seq
                                (seq
                                 (seq
                                  (call %init_peer_id% ("get-dt" "get_dt") [name] results-0)
                                  (ap results-0.$.field results-0_flat)
                                 )
                                 (ap results-0_flat $results)
                                )
                                (call %init_peer_id% ("get-dt" "get_dt") [name] results-1)
                               )
                               (ap results-1.$.field results-1_flat)
                              )
                              (ap results-1_flat $results)
                             )
                             (call %init_peer_id% ("get-dt" "get_dt") [name] results-2)
                            )
                            (ap results-2.$.field results-2_flat)
                           )
                           (ap results-2_flat $results)
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
 

export function use_name2(
    name: string,
    config?: {ttl?: number}
): Promise<string[]>;

export function use_name2(
    peer: IFluenceClient$$,
    name: string,
    config?: {ttl?: number}
): Promise<string[]>;

export function use_name2(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "use_name2",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
                "name" : {
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
        use_name2_script
    )
}

/* eslint-enable */