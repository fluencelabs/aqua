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

export interface GetArrDef {
    getArr: (callParams: CallParams$$<null>) => string[] | Promise<string[]>;
}
export function registerGetArr(service: GetArrDef): void;
export function registerGetArr(serviceId: string, service: GetArrDef): void;
export function registerGetArr(peer: IFluenceClient$$, service: GetArrDef): void;
export function registerGetArr(peer: IFluenceClient$$, serviceId: string, service: GetArrDef): void;
       

export function registerGetArr(...args: any) {
    registerService$$(
        args,
        {
    "defaultServiceId" : "getArr",
    "functions" : {
        "tag" : "labeledProduct",
        "fields" : {
            "getArr" : {
                "tag" : "arrow",
                "domain" : {
                    "tag" : "nil"
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
            }
        }
    }
}
    );
}
      


export interface OpODef {
    identity: (arg0: string, callParams: CallParams$$<'arg0'>) => string | Promise<string>;
}
export function registerOpO(service: OpODef): void;
export function registerOpO(serviceId: string, service: OpODef): void;
export function registerOpO(peer: IFluenceClient$$, service: OpODef): void;
export function registerOpO(peer: IFluenceClient$$, serviceId: string, service: OpODef): void;
       

export function registerOpO(...args: any) {
    registerService$$(
        args,
        {
    "defaultServiceId" : "op",
    "functions" : {
        "tag" : "labeledProduct",
        "fields" : {
            "identity" : {
                "tag" : "arrow",
                "domain" : {
                    "tag" : "unlabeledProduct",
                    "items" : [
                        {
                            "tag" : "scalar",
                            "name" : "string"
                        }
                    ]
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
export const streamSugar_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (seq
                         (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                         (call %init_peer_id% ("getDataSrv" "n") [] n)
                        )
                        (call %init_peer_id% ("getDataSrv" "m") [] m)
                       )
                       (new $str
                        (new $arr
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
                                   (seq
                                    (ap 1 $arr)
                                    (ap 2 $arr)
                                   )
                                   (ap n $arr)
                                  )
                                  (ap 4 $stream-inline)
                                 )
                                 (ap 5 $stream-inline)
                                )
                                (ap m $stream-inline)
                               )
                               (canon %init_peer_id% $stream-inline  #stream-inline_canon)
                              )
                              (fold #stream-inline_canon i-0
                               (seq
                                (ap i-0 $str)
                                (next i-0)
                               )
                              )
                             )
                             (canon %init_peer_id% $arr  #-arr-fix-0)
                            )
                            (ap #-arr-fix-0 -arr-flat-0)
                           )
                           (canon %init_peer_id% $str  #-str-fix-1)
                          )
                          (ap #-str-fix-1 -str-flat-1)
                         )
                        )
                       )
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [-arr-flat-0 -str-flat-1])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 
export type StreamSugarResult = [number[], number[]]
export function streamSugar(
    n: number,
    m: number,
    config?: {ttl?: number}
): Promise<StreamSugarResult>;

export function streamSugar(
    peer: IFluenceClient$$,
    n: number,
    m: number,
    config?: {ttl?: number}
): Promise<StreamSugarResult>;

export function streamSugar(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "streamSugar",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
                "n" : {
                    "tag" : "scalar",
                    "name" : "u32"
                },
                "m" : {
                    "tag" : "scalar",
                    "name" : "u32"
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
                        "name" : "u32"
                    }
                },
                {
                    "tag" : "array",
                    "type" : {
                        "tag" : "scalar",
                        "name" : "u32"
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
        streamSugar_script
    )
}

export const optionSugar_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (seq
                         (seq
                          (seq
                           (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                           (call %init_peer_id% ("getDataSrv" "numSome") [] numSome)
                          )
                          (call %init_peer_id% ("getDataSrv" "strSome") [] strSome)
                         )
                         (call %init_peer_id% ("getDataSrv" "numNone") [] numNone)
                        )
                        (call %init_peer_id% ("getDataSrv" "strNone") [] strNone)
                       )
                       (new $str
                        (seq
                         (seq
                          (seq
                           (seq
                            (seq
                             (seq
                              (seq
                               (new $option-inline
                                (seq
                                 (xor
                                  (xor
                                   (ap numNone.$.[0] $option-inline)
                                   (ap numSome.$.[0] $option-inline)
                                  )
                                  (null)
                                 )
                                 (canon %init_peer_id% $option-inline  #option-inline-0)
                                )
                               )
                               (new $option-inline-1
                                (seq
                                 (xor
                                  (xor
                                   (xor
                                    (xor
                                     (xor
                                      (ap strNone.$.[0] $option-inline-1)
                                      (ap strNone.$.[0] $option-inline-1)
                                     )
                                     (ap strNone.$.[0] $option-inline-1)
                                    )
                                    (ap strNone.$.[0] $option-inline-1)
                                   )
                                   (ap strNone.$.[0] $option-inline-1)
                                  )
                                  (null)
                                 )
                                 (canon %init_peer_id% $option-inline-1  #option-inline-1-0)
                                )
                               )
                              )
                              (new $option-inline-2
                               (seq
                                (xor
                                 (xor
                                  (xor
                                   (ap strSome.$.[0] $option-inline-2)
                                   (ap strNone.$.[0] $option-inline-2)
                                  )
                                  (ap "random string" $option-inline-2)
                                 )
                                 (null)
                                )
                                (canon %init_peer_id% $option-inline-2  #option-inline-2-0)
                               )
                              )
                             )
                             (fold #option-inline-2-0 i-0
                              (seq
                               (ap i-0 $str)
                               (next i-0)
                              )
                             )
                            )
                            (new $option-inline-3
                             (seq
                              (xor
                               (xor
                                (ap strNone.$.[0] $option-inline-3)
                                (ap strNone.$.[0] $option-inline-3)
                               )
                               (null)
                              )
                              (canon %init_peer_id% $option-inline-3  #option-inline-3-0)
                             )
                            )
                           )
                           (fold #option-inline-3-0 i-1
                            (seq
                             (ap i-1 $str)
                             (next i-1)
                            )
                           )
                          )
                          (canon %init_peer_id% $str  #-str-fix-0)
                         )
                         (ap #-str-fix-0 -str-flat-0)
                        )
                       )
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [#option-inline-0 -str-flat-0 #option-inline-1-0])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 
export type OptionSugarResult = [number[], string[], string[]]
export function optionSugar(
    numSome: number | null,
    strSome: string | null,
    numNone: number | null,
    strNone: string | null,
    config?: {ttl?: number}
): Promise<OptionSugarResult>;

export function optionSugar(
    peer: IFluenceClient$$,
    numSome: number | null,
    strSome: string | null,
    numNone: number | null,
    strNone: string | null,
    config?: {ttl?: number}
): Promise<OptionSugarResult>;

export function optionSugar(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "optionSugar",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
                "numSome" : {
                    "tag" : "option",
                    "type" : {
                        "tag" : "scalar",
                        "name" : "u32"
                    }
                },
                "strSome" : {
                    "tag" : "option",
                    "type" : {
                        "tag" : "scalar",
                        "name" : "string"
                    }
                },
                "numNone" : {
                    "tag" : "option",
                    "type" : {
                        "tag" : "scalar",
                        "name" : "u32"
                    }
                },
                "strNone" : {
                    "tag" : "option",
                    "type" : {
                        "tag" : "scalar",
                        "name" : "string"
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
                        "name" : "u32"
                    }
                },
                {
                    "tag" : "array",
                    "type" : {
                        "tag" : "scalar",
                        "name" : "string"
                    }
                },
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
        optionSugar_script
    )
}

export const getNeighbours_script = `
                    (xor
                     (seq
                      (seq
                       (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                       (call %init_peer_id% ("getArr" "getArr") [] nodes)
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [nodes])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function getNeighbours(
    config?: {ttl?: number}
): Promise<string[]>;

export function getNeighbours(
    peer: IFluenceClient$$,
    config?: {ttl?: number}
): Promise<string[]>;

export function getNeighbours(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "getNeighbours",
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
        getNeighbours_script
    )
}

export const bugLNG59_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (seq
                         (seq
                          (seq
                           (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                           (call %init_peer_id% ("getArr" "getArr") [] nodes)
                          )
                          (new $array-inline
                           (seq
                            (ap -relay- $array-inline)
                            (canon %init_peer_id% $array-inline  #array-inline-0)
                           )
                          )
                         )
                         (call -relay- ("op" "noop") [])
                        )
                        (fold #array-inline-0 -via-peer-
                         (seq
                          (call -via-peer- ("op" "noop") [])
                          (next -via-peer-)
                         )
                        )
                       )
                       (xor
                        (seq
                         (seq
                          (call nodes.$.[1] ("op" "identity") ["some str"] res)
                          (fold #array-inline-0 -via-peer-
                           (seq
                            (next -via-peer-)
                            (call -via-peer- ("op" "noop") [])
                           )
                          )
                         )
                         (call -relay- ("op" "noop") [])
                        )
                        (seq
                         (seq
                          (fold #array-inline-0 -via-peer-
                           (seq
                            (call -via-peer- ("op" "noop") [])
                            (next -via-peer-)
                           )
                          )
                          (call -relay- ("op" "noop") [])
                         )
                         (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                        )
                       )
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [res])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 3])
                    )
    `
 

export function bugLNG59(
    config?: {ttl?: number}
): Promise<string>;

export function bugLNG59(
    peer: IFluenceClient$$,
    config?: {ttl?: number}
): Promise<string>;

export function bugLNG59(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "bugLNG59",
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
        bugLNG59_script
    )
}

export const arraySugar_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (seq
                         (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                         (call %init_peer_id% ("getDataSrv" "n") [] n)
                        )
                        (call %init_peer_id% ("getDataSrv" "m") [] m)
                       )
                       (new $str
                        (seq
                         (seq
                          (seq
                           (seq
                            (new $array-inline
                             (seq
                              (seq
                               (seq
                                (ap 1 $array-inline)
                                (ap 2 $array-inline)
                               )
                               (ap n $array-inline)
                              )
                              (canon %init_peer_id% $array-inline  #array-inline-0)
                             )
                            )
                            (new $array-inline-1
                             (seq
                              (seq
                               (seq
                                (ap 4 $array-inline-1)
                                (ap 5 $array-inline-1)
                               )
                               (ap m $array-inline-1)
                              )
                              (canon %init_peer_id% $array-inline-1  #array-inline-1-0)
                             )
                            )
                           )
                           (fold #array-inline-1-0 i-0
                            (seq
                             (ap i-0 $str)
                             (next i-0)
                            )
                           )
                          )
                          (canon %init_peer_id% $str  #-str-fix-0)
                         )
                         (ap #-str-fix-0 -str-flat-0)
                        )
                       )
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [#array-inline-0 -str-flat-0])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 
export type ArraySugarResult = [number[], number[]]
export function arraySugar(
    n: number,
    m: number,
    config?: {ttl?: number}
): Promise<ArraySugarResult>;

export function arraySugar(
    peer: IFluenceClient$$,
    n: number,
    m: number,
    config?: {ttl?: number}
): Promise<ArraySugarResult>;

export function arraySugar(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "arraySugar",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
                "n" : {
                    "tag" : "scalar",
                    "name" : "u32"
                },
                "m" : {
                    "tag" : "scalar",
                    "name" : "u32"
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
                        "name" : "u32"
                    }
                },
                {
                    "tag" : "array",
                    "type" : {
                        "tag" : "scalar",
                        "name" : "u32"
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
        arraySugar_script
    )
}

/* eslint-enable */