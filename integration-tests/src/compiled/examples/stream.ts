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

export interface StringerDef {
    returnString: (arg0: string, callParams: CallParams$$<'arg0'>) => string | Promise<string>;
}
export function registerStringer(service: StringerDef): void;
export function registerStringer(serviceId: string, service: StringerDef): void;
export function registerStringer(peer: IFluenceClient$$, service: StringerDef): void;
export function registerStringer(peer: IFluenceClient$$, serviceId: string, service: StringerDef): void;
       

export function registerStringer(...args: any) {
    registerService$$(
        args,
        {
    "defaultServiceId" : "stringer-id",
    "functions" : {
        "tag" : "labeledProduct",
        "fields" : {
            "returnString" : {
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
export const returnNone_script = `
                    (xor
                     (seq
                      (seq
                       (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                       (new $valueNone
                        (seq
                         (canon %init_peer_id% $valueNone  #-valueNone-fix-0)
                         (ap #-valueNone-fix-0 -valueNone-flat-0)
                        )
                       )
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [-valueNone-flat-0])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function returnNone(
    config?: {ttl?: number}
): Promise<string | null>;

export function returnNone(
    peer: IFluenceClient$$,
    config?: {ttl?: number}
): Promise<string | null>;

export function returnNone(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "returnNone",
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
                    "tag" : "option",
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
        returnNone_script
    )
}

export const returnNilLength_script = `
                    (xor
                     (seq
                      (seq
                       (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                       (new $arr
                        (seq
                         (seq
                          (seq
                           (ap [] literal_ap)
                           (ap literal_ap literal_props)
                          )
                          (ap literal_props literal_props_to_functor)
                         )
                         (ap literal_props_to_functor.length literal_props_length)
                        )
                       )
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [literal_props_length])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function returnNilLength(
    config?: {ttl?: number}
): Promise<number>;

export function returnNilLength(
    peer: IFluenceClient$$,
    config?: {ttl?: number}
): Promise<number>;

export function returnNilLength(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "returnNilLength",
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
                    "name" : "u32"
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
        returnNilLength_script
    )
}

export const stringNone_script = `
                    (xor
                     (seq
                      (seq
                       (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                       (new $valueNone
                        (seq
                         (canon %init_peer_id% $valueNone  #-valueNone-fix-0)
                         (ap #-valueNone-fix-0 -valueNone-flat-0)
                        )
                       )
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [-valueNone-flat-0])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function stringNone(
    config?: {ttl?: number}
): Promise<string | null>;

export function stringNone(
    peer: IFluenceClient$$,
    config?: {ttl?: number}
): Promise<string | null>;

export function stringNone(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "stringNone",
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
                    "tag" : "option",
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
        stringNone_script
    )
}

export const streamIntFunctor_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                        (call %init_peer_id% ("getDataSrv" "arr") [] arr)
                       )
                       (new $stream
                        (seq
                         (seq
                          (seq
                           (new $array-inline
                            (seq
                             (ap "123" $array-inline)
                             (canon %init_peer_id% $array-inline  #array-inline-0)
                            )
                           )
                           (ap #array-inline-0 $stream)
                          )
                          (new $stream_test
                           (seq
                            (seq
                             (seq
                              (call %init_peer_id% ("math" "add") [arr.$.[0] 1] stream_incr)
                              (fold $stream stream_fold_var
                               (seq
                                (seq
                                 (ap stream_fold_var $stream_test)
                                 (canon %init_peer_id% $stream_test  #stream_iter_canon)
                                )
                                (xor
                                 (match #stream_iter_canon.length stream_incr
                                  (null)
                                 )
                                 (next stream_fold_var)
                                )
                               )
                               (never)
                              )
                             )
                             (canon %init_peer_id% $stream_test  #stream_result_canon)
                            )
                            (ap #stream_result_canon stream_gate)
                           )
                          )
                         )
                         (par
                          (seq
                           (ap arr.$.[0] arr_flat)
                           (call %init_peer_id% ("op" "noop") [])
                          )
                          (seq
                           (ap arr.$.[0] arr_flat-0)
                           (call %init_peer_id% ("op" "noop") [])
                          )
                         )
                        )
                       )
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [stream_gate.$.[arr_flat].[arr_flat-0]])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function streamIntFunctor(
    arr: number[],
    config?: {ttl?: number}
): Promise<string>;

export function streamIntFunctor(
    peer: IFluenceClient$$,
    arr: number[],
    config?: {ttl?: number}
): Promise<string>;

export function streamIntFunctor(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "streamIntFunctor",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
                "arr" : {
                    "tag" : "array",
                    "type" : {
                        "tag" : "scalar",
                        "name" : "u32"
                    }
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
        streamIntFunctor_script
    )
}

export const returnNilLiteral_script = `
                    (xor
                     (seq
                      (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [[]])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function returnNilLiteral(
    config?: {ttl?: number}
): Promise<string[]>;

export function returnNilLiteral(
    peer: IFluenceClient$$,
    config?: {ttl?: number}
): Promise<string[]>;

export function returnNilLiteral(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "returnNilLiteral",
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
        returnNilLiteral_script
    )
}

export const returnStreamFromFunc_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (seq
                         (seq
                          (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                          (ap 1 $nums)
                         )
                         (ap 2 $nums)
                        )
                        (ap 3 $nums)
                       )
                       (ap 4 $nums)
                      )
                      (xor
                       (seq
                        (canon %init_peer_id% $nums  #nums_canon)
                        (call %init_peer_id% ("callbackSrv" "response") [#nums_canon])
                       )
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function returnStreamFromFunc(
    config?: {ttl?: number}
): Promise<number[]>;

export function returnStreamFromFunc(
    peer: IFluenceClient$$,
    config?: {ttl?: number}
): Promise<number[]>;

export function returnStreamFromFunc(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "returnStreamFromFunc",
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
        returnStreamFromFunc_script
    )
}

export const returnNil_script = `
                    (xor
                     (seq
                      (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                      (xor
                       (seq
                        (canon %init_peer_id% $valueNil  #valueNil_canon)
                        (call %init_peer_id% ("callbackSrv" "response") [#valueNil_canon])
                       )
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function returnNil(
    config?: {ttl?: number}
): Promise<string[]>;

export function returnNil(
    peer: IFluenceClient$$,
    config?: {ttl?: number}
): Promise<string[]>;

export function returnNil(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "returnNil",
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
        returnNil_script
    )
}

export const stringNil_script = `
                    (xor
                     (seq
                      (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                      (xor
                       (seq
                        (canon %init_peer_id% $valueNil  #valueNil_canon)
                        (call %init_peer_id% ("callbackSrv" "response") [#valueNil_canon])
                       )
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function stringNil(
    config?: {ttl?: number}
): Promise<string[]>;

export function stringNil(
    peer: IFluenceClient$$,
    config?: {ttl?: number}
): Promise<string[]>;

export function stringNil(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "stringNil",
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
        stringNil_script
    )
}

export const checkStreams_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                        (call %init_peer_id% ("getDataSrv" "ch") [] ch)
                       )
                       (new $stream
                        (seq
                         (seq
                          (seq
                           (seq
                            (call %init_peer_id% ("stringer-id" "returnString") ["first"] $stream)
                            (call %init_peer_id% ("stringer-id" "returnString") ["second"] $stream)
                           )
                           (fold ch b-0
                            (seq
                             (call %init_peer_id% ("stringer-id" "returnString") [b-0] $stream)
                             (next b-0)
                            )
                           )
                          )
                          (canon %init_peer_id% $stream  #-stream-fix-0)
                         )
                         (ap #-stream-fix-0 -stream-flat-0)
                        )
                       )
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [-stream-flat-0])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function checkStreams(
    ch: string[],
    config?: {ttl?: number}
): Promise<string[]>;

export function checkStreams(
    peer: IFluenceClient$$,
    ch: string[],
    config?: {ttl?: number}
): Promise<string[]>;

export function checkStreams(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "checkStreams",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
                "ch" : {
                    "tag" : "array",
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
        checkStreams_script
    )
}

export const streamAssignment_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                        (call %init_peer_id% ("getDataSrv" "arr") [] arr)
                       )
                       (new $stream
                        (seq
                         (seq
                          (seq
                           (seq
                            (seq
                             (seq
                              (seq
                               (seq
                                (seq
                                 (new $array-inline
                                  (seq
                                   (ap 0 $array-inline)
                                   (canon %init_peer_id% $array-inline  #array-inline-0)
                                  )
                                 )
                                 (ap #array-inline-0 $stream)
                                )
                                (ap arr arr_to_functor)
                               )
                               (ap arr_to_functor.length arr_length)
                              )
                              (call %init_peer_id% ("math" "sub") [arr_length 1] sub)
                             )
                             (new $stream_test
                              (seq
                               (seq
                                (seq
                                 (call %init_peer_id% ("math" "add") [sub 1] stream_incr)
                                 (fold $stream stream_fold_var
                                  (seq
                                   (seq
                                    (ap stream_fold_var $stream_test)
                                    (canon %init_peer_id% $stream_test  #stream_iter_canon)
                                   )
                                   (xor
                                    (match #stream_iter_canon.length stream_incr
                                     (null)
                                    )
                                    (next stream_fold_var)
                                   )
                                  )
                                  (never)
                                 )
                                )
                                (canon %init_peer_id% $stream_test  #stream_result_canon)
                               )
                               (ap #stream_result_canon stream_gate)
                              )
                             )
                            )
                            (ap arr arr_to_functor-0)
                           )
                           (ap arr_to_functor-0.length arr_length-0)
                          )
                          (call %init_peer_id% ("math" "sub") [arr_length-0 1] sub-0)
                         )
                         (ap stream_gate.$.[sub-0].[0] stream_gate_flat)
                        )
                       )
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [arr.$.[stream_gate_flat]])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function streamAssignment(
    arr: string[],
    config?: {ttl?: number}
): Promise<string>;

export function streamAssignment(
    peer: IFluenceClient$$,
    arr: string[],
    config?: {ttl?: number}
): Promise<string>;

export function streamAssignment(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "streamAssignment",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
                "arr" : {
                    "tag" : "array",
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
        streamAssignment_script
    )
}

export const streamFunctor_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                        (call %init_peer_id% ("getDataSrv" "arr") [] arr)
                       )
                       (new $stream
                        (seq
                         (seq
                          (seq
                           (seq
                            (seq
                             (seq
                              (seq
                               (seq
                                (new $array-inline
                                 (seq
                                  (ap "123" $array-inline)
                                  (canon %init_peer_id% $array-inline  #array-inline-0)
                                 )
                                )
                                (ap #array-inline-0 $stream)
                               )
                               (ap arr arr_to_functor)
                              )
                              (ap arr_to_functor.length arr_length)
                             )
                             (call %init_peer_id% ("math" "sub") [arr_length 1] sub)
                            )
                            (new $stream_test
                             (seq
                              (seq
                               (seq
                                (call %init_peer_id% ("math" "add") [sub 1] stream_incr)
                                (fold $stream stream_fold_var
                                 (seq
                                  (seq
                                   (ap stream_fold_var $stream_test)
                                   (canon %init_peer_id% $stream_test  #stream_iter_canon)
                                  )
                                  (xor
                                   (match #stream_iter_canon.length stream_incr
                                    (null)
                                   )
                                   (next stream_fold_var)
                                  )
                                 )
                                 (never)
                                )
                               )
                               (canon %init_peer_id% $stream_test  #stream_result_canon)
                              )
                              (ap #stream_result_canon stream_gate)
                             )
                            )
                           )
                           (ap arr arr_to_functor-0)
                          )
                          (ap arr_to_functor-0.length arr_length-0)
                         )
                         (call %init_peer_id% ("math" "sub") [arr_length-0 1] sub-0)
                        )
                       )
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [stream_gate.$.[sub-0].[0]])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function streamFunctor(
    arr: string[],
    config?: {ttl?: number}
): Promise<string>;

export function streamFunctor(
    peer: IFluenceClient$$,
    arr: string[],
    config?: {ttl?: number}
): Promise<string>;

export function streamFunctor(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "streamFunctor",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
                "arr" : {
                    "tag" : "array",
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
        streamFunctor_script
    )
}

export const getStream_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (seq
                         (seq
                          (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                          (ap 1 $nums)
                         )
                         (ap 2 $nums)
                        )
                        (ap 3 $nums)
                       )
                       (ap 4 $nums)
                      )
                      (xor
                       (seq
                        (canon %init_peer_id% $nums  #nums_canon)
                        (call %init_peer_id% ("callbackSrv" "response") [#nums_canon])
                       )
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function getStream(
    config?: {ttl?: number}
): Promise<number[]>;

export function getStream(
    peer: IFluenceClient$$,
    config?: {ttl?: number}
): Promise<number[]>;

export function getStream(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "getStream",
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
        getStream_script
    )
}

export const streamJoin_script = `
                    (xor
                     (seq
                      (seq
                       (seq
                        (call %init_peer_id% ("getDataSrv" "-relay-") [] -relay-)
                        (call %init_peer_id% ("getDataSrv" "arr") [] arr)
                       )
                       (new $streamJ
                        (seq
                         (seq
                          (seq
                           (seq
                            (seq
                             (seq
                              (seq
                               (seq
                                (new $array-inline
                                 (seq
                                  (seq
                                   (ap "111" $array-inline)
                                   (ap "222" $array-inline)
                                  )
                                  (canon %init_peer_id% $array-inline  #array-inline-0)
                                 )
                                )
                                (ap #array-inline-0 $streamJ)
                               )
                               (new $array-inline-1
                                (seq
                                 (seq
                                  (ap "333" $array-inline-1)
                                  (ap "444" $array-inline-1)
                                 )
                                 (canon %init_peer_id% $array-inline-1  #array-inline-1-0)
                                )
                               )
                              )
                              (ap #array-inline-1-0 $streamJ)
                             )
                             (ap arr arr_to_functor)
                            )
                            (ap arr_to_functor.length arr_length)
                           )
                           (new $streamJ_test
                            (seq
                             (seq
                              (seq
                               (call %init_peer_id% ("math" "add") [arr_length 1] streamJ_incr)
                               (fold $streamJ streamJ_fold_var
                                (seq
                                 (seq
                                  (ap streamJ_fold_var $streamJ_test)
                                  (canon %init_peer_id% $streamJ_test  #streamJ_iter_canon)
                                 )
                                 (xor
                                  (match #streamJ_iter_canon.length streamJ_incr
                                   (null)
                                  )
                                  (next streamJ_fold_var)
                                 )
                                )
                                (never)
                               )
                              )
                              (canon %init_peer_id% $streamJ_test  #streamJ_result_canon)
                             )
                             (ap #streamJ_result_canon streamJ_gate)
                            )
                           )
                          )
                          (ap arr arr_to_functor-0)
                         )
                         (ap arr_to_functor-0.length arr_length-0)
                        )
                       )
                      )
                      (xor
                       (call %init_peer_id% ("callbackSrv" "response") [streamJ_gate.$.[arr_length-0].[1]])
                       (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 1])
                      )
                     )
                     (call %init_peer_id% ("errorHandlingSrv" "error") [%last_error% 2])
                    )
    `
 

export function streamJoin(
    arr: string[],
    config?: {ttl?: number}
): Promise<string>;

export function streamJoin(
    peer: IFluenceClient$$,
    arr: string[],
    config?: {ttl?: number}
): Promise<string>;

export function streamJoin(...args: any) {


    return callFunction$$(
        args,
        {
    "functionName" : "streamJoin",
    "arrow" : {
        "tag" : "arrow",
        "domain" : {
            "tag" : "labeledProduct",
            "fields" : {
                "arr" : {
                    "tag" : "array",
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
        streamJoin_script
    )
}

/* eslint-enable */