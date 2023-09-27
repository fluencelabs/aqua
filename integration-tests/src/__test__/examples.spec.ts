import { Fluence, IFluenceClient, createClient } from "@fluencelabs/js-client";
import {
  getObjAssignCall,
  getObjCall,
  getObjRelayCall,
  getObjForCall,
} from "../examples/objectCall.js";
import {
  callArrowCall,
  reproArgsBug426Call,
} from "../examples/callArrowCall.js";
import { dataAliasCall } from "../examples/dataAliasCall.js";
import { onCall } from "../examples/onCall.js";
import {
  onPropagateCall,
  nestedOnPropagateCall,
  seqOnPropagateCall,
} from "../examples/onErrorPropagation.js";
import { errorClearCall } from "../examples/errorClear.js";
import { handleResultErrorCall } from "../examples/handleResultError.js";
import { funcCall } from "../examples/funcCall.js";
import { registerPrintln } from "../compiled/examples/println.js";
import { helloWorldCall } from "../examples/helloWorldCall.js";
import { foldBug499Call, foldCall } from "../examples/foldCall.js";
import { bugNG69Call, ifCall, ifWrapCall } from "../examples/ifCall.js";
import { ifPropagateErrorsCall } from "../examples/ifPropagateErrors.js";
import { parCall, testTimeoutCall } from "../examples/parCall.js";
import { complexCall } from "../examples/complex.js";
import {
  constantsCall,
  particleTtlAndTimestampCall,
} from "../examples/constantsCall.js";
import {
  abilityCall,
  complexAbilityCall,
  checkAbCallsCall,
} from "../examples/abilityCall.js";
import {
  nilLengthCall,
  nilLiteralCall,
  returnNilCall,
  returnNoneCall,
  streamAssignmentCall,
  streamCall,
  streamFunctorCall,
  streamIntFunctorCall,
  streamJoinCall,
  streamReturnFromInnerFunc,
} from "../examples/streamCall.js";
import {
  topologyBug205Call,
  topologyBug394Call,
  topologyBug427Call,
  topologyCall,
} from "../examples/topologyCall.js";
import { foldJoinCall } from "../examples/foldJoinCall.js";
import {
  registerHandlers,
  returnNull,
  returnOptionalCall,
  useOptionalCall,
} from "../examples/useOptionalCall.js";
import {
  viaArrCall,
  viaOptCall,
  viaOptNullCall,
  viaStreamCall,
} from "../examples/viaCall.js";
import { nestedFuncsCall } from "../examples/nestedFuncsCall.js";
import { assignmentCall } from "../examples/assignment.js";
import {
  boolAlgebraCall,
  compareStreamsCall,
  compareStructsCall,
} from "../examples/boolAlgebra.js";
import { tryCatchCall } from "../examples/tryCatchCall.js";
import { tryOtherwiseCall } from "../examples/tryOtherwiseCall.js";
import { coCall } from "../examples/coCall.js";
import { bugLNG60Call, passArgsCall } from "../examples/passArgsCall.js";
import { streamArgsCall } from "../examples/streamArgsCall.js";
import { streamResultsCall } from "../examples/streamResultsCall.js";
import { structuralTypingCall } from "../examples/structuralTypingCall";
import {
  servicesAsAbilitiesCall,
  expectedServiceResults,
  servicesAsAbilitiesCaptureCall,
  expectedServiceCaptureResults,
} from "../examples/servicesAsAbilities.js";
import { streamReturnCall } from "../examples/streamReturn.js";
import {
  streamCaptureSimpleCall,
  streamCaptureReturnCall,
} from "../examples/streamCapture.js";
import {
  streamIfCall,
  streamForCall,
  streamTryCall,
  streamComplexCall,
} from "../examples/streamScopes.js";
import { pushToStreamCall } from "../examples/pushToStreamCall.js";
import { literalCall } from "../examples/returnLiteralCall.js";
import { multiReturnCall } from "../examples/multiReturnCall.js";
import { declareCall } from "../examples/declareCall.js";
import { genOptions, genOptionsEmptyString } from "../examples/optionsCall.js";
import { lng193BugCall } from "../examples/closureReturnRename.js";
import { closuresCall } from "../examples/closures.js";
import { closureArrowCaptureCall } from "../examples/closureArrowCapture.js";
import {
  bugLNG63_2Call,
  bugLNG63_3Call,
  bugLNG63Call,
  streamCanCall,
} from "../examples/streamCanCall.js";
import { streamCallbackCall } from "../examples/streamCallback.js";
import { streamResCall } from "../examples/streamRestrictionsCall.js";
import {
  joinIdxCall,
  joinIdxLocalCall,
  joinIdxRelayCall,
} from "../examples/joinCall.js";
import { recursiveStreamsCall } from "../examples/recursiveStreamsCall.js";
import { renameVarsCall } from "../examples/renameVars.js";
import {
  arraySugarCall,
  bugLNG59Call,
  optionSugarCall,
  streamSugarCall,
} from "../examples/collectionSugarCall.js";
import { funcsCall } from "../examples/funcsCall.js";
import { nestedDataCall } from "../examples/nestedDataCall.js";
import {
  mathTest1Call,
  mathTest2Call,
  mathTestI16Call,
  mathTestI32Call,
  mathTestI64Call,
  mathTestU64Call,
} from "../examples/mathCall.js";
import { lng58Bug } from "../compiled/examples/closures.js";
import { config, isEphemeral } from "../config.js";
import { bugLng79Call } from "../examples/canonCall.js";
import { bugLng119Call } from "../examples/functorsCall.js";
import {
  returnArrowCall,
  returnArrowChainCall,
} from "../examples/returnArrowCall.js";

var selfPeerId: string;
var peer1: IFluenceClient;
var peer2: IFluenceClient;

export const relay1 = config.relays[0];
const relayPeerId1 = relay1.peerId;
export const relay2 = config.relays[1];
const relayPeerId2 = relay2.peerId;

import log from "loglevel";

// log.setDefaultLevel("debug")

async function start() {
  console.log("CONNECTING TO FIRST:");
  Fluence.onConnectionStateChange((s) => {
    console.log(s);
  });
  await Fluence.connect(relay1, {});
  const cl = await Fluence.getClient();
  peer1 = cl;
  selfPeerId = cl.getPeerId();
  console.log("CONNECTED");

  peer2 = await createClient(relay2, {});
  console.log("CONNECTING TO SECOND:");
  peer2.onConnectionStateChange((s) => {
    console.log(s);
  });
  console.log("CONNECTED");
}

async function stop() {
  await Fluence.disconnect();
  if (peer2) {
    await peer2.disconnect();
  }
}

describe("Testing examples", () => {
  beforeAll(async () => {
    await start();

    // this could be called from `println.aqua`
    registerPrintln({
      print: (arg0) => {
        console.dir(arg0);
      },
    });
  }, 12000);

  afterAll(async () => {
    await stop();
  });

  it("callArrow.aqua args bug 426", async () => {
    let argResult = await reproArgsBug426Call();

    expect(argResult).toBe("privet");
  });

  it("returnArrow.aqua", async () => {
    let [result1, result2] = await returnArrowCall();

    expect(result1).toBe("arg for closure ");
    expect(result2).toBe("arg for closure arg for func  literal");
  });

  it("returnArrow.aqua chain", async () => {
    let argResult = await returnArrowChainCall();

    expect(argResult).toStrictEqual([
      "first",
      "firstarg for func1  literal",
      "second",
      "secondarg for func2  second literal",
      "third",
      "thirdarg for func2  second literal",
      "fourth",
      "fourth from second literal",
    ]);
  });

  it("streamRestrictions.aqua", async () => {
    let streamResResult = await streamResCall();

    expect(streamResResult).toEqual([[], ["a", "b", "c"]]);
  });

  it("streamScopes.aqua streamIf", async () => {
    let streamIfResult = await streamIfCall();

    expect(streamIfResult).toEqual(5);
  });

  it("streamScopes.aqua streamTry", async () => {
    let streamTryResult = await streamTryCall();

    expect(streamTryResult).toEqual(4);
  });

  it("streamScopes.aqua streamFor", async () => {
    let streamTryResult = await streamForCall();

    expect(streamTryResult).toEqual(4);
  });

  it("streamScopes.aqua streamComplex", async () => {
    let streamTryResult = await streamComplexCall();

    expect(streamTryResult).toEqual(13);
  });

  it("if.aqua", async () => {
    await ifCall();
  });

  it("if.aqua xor wrap", async () => {
    let res = await ifWrapCall(relay2.peerId);
    expect(res).toBe("1x");
  });

  it("if.aqua bug LNG-69", async () => {
    let res = await bugNG69Call(relay2.peerId);
    expect(res).toBe(true);
  });

  it("ifPropagateErrors.aqua", async () => {
    let res = await ifPropagateErrorsCall();
    expect(res).toEqual([1, 2, 3].map((i) => "otherwise" + i));
  });

  it("helloWorld.aqua", async () => {
    let helloWorldResult = await helloWorldCall();
    expect(helloWorldResult).toBe("Hello, NAME!");
  });

  it("func.aqua", async () => {
    let funcCallResult = await funcCall();
    expect(funcCallResult).toBe("some str");
  });

  it("dataAlias.aqua", async () => {
    let dataAliasResult = await dataAliasCall();
    expect(dataAliasResult).toBe("peer id str");
  });

  it("constants.aqua", async () => {
    let constantCallResult = await constantsCall();
    expect(constantCallResult).toEqual(["5", "default-str"]);
  });

  it("PARTICLE_TTL and PARTICLE_TIMESTAMP", async () => {
    const ttl = 1234;
    let result = await particleTtlAndTimestampCall(ttl);
    expect(result[1]).toBeDefined();
    expect(result[0]).toEqual(ttl);
  });

  it("stream.aqua return stream from inner func", async () => {
    let streamResult = await streamReturnFromInnerFunc();
    expect(streamResult).toEqual([1, 2, 3, 4]);
  });

  it("stream.aqua functor", async () => {
    let streamResult = await streamFunctorCall();
    expect(streamResult).toEqual("123");
  });

  it("stream.aqua assignment", async () => {
    let streamResult = await streamAssignmentCall();
    expect(streamResult).toEqual("333");
  });

  it("stream.aqua nil literal", async () => {
    let result = await nilLiteralCall();
    expect(result).toEqual([]);
  });

  it("structuraltyping.aqua", async () => {
    let result = await structuralTypingCall();
    expect(result).toEqual("some_stringsome_stringsome_stringab_string");
  });

  it("servicesAsAbilities.aqua", async () => {
    let result = await servicesAsAbilitiesCall();
    expect(result).toEqual(expectedServiceResults);
  });

  it("servicesAsAbilities.aqua capture", async () => {
    let result = await servicesAsAbilitiesCaptureCall();
    expect(result).toEqual(expectedServiceCaptureResults);
  });

  it("collectionSugar array", async () => {
    let result = await arraySugarCall();
    expect(result).toEqual([
      [1, 2, 3],
      [4, 5, 6],
    ]);
  });

  it("object creation getObj", async () => {
    let result = await getObjCall();
    expect(result).toEqual({
      str: "some str",
      num: 5,
      inner: {
        arr: ["a", "b", "c"],
        num: 6,
      },
    });
  });

  it("object creation in 'for' instruction getObjFor", async () => {
    const result = await getObjForCall();
    const res = [
      {
        str: "first copied",
        num: 1,
        inner: {
          arr: ["copy"],
          num: 11,
        },
      },
      {
        str: "second copied",
        num: 2,
        inner: {
          arr: ["copy"],
          num: 22,
        },
      },
      {
        str: "third copied",
        num: 3,
        inner: {
          arr: ["copy"],
          num: 33,
        },
      },
      {
        str: "for",
        num: 1,
        inner: {
          arr: [],
          num: 1,
        },
      },
      {
        str: "for",
        num: 2,
        inner: {
          arr: [],
          num: 2,
        },
      },
      {
        str: "for",
        num: 3,
        inner: {
          arr: [],
          num: 3,
        },
      },
    ];
    expect(result).toEqual(res);
  });

  it("object creation getObjAssign", async () => {
    let result = await getObjAssignCall();
    expect(result).toEqual([
      {
        str: "first str",
        num: 5,
        inner: {
          arr: ["d", "e", "f"],
          num: 7,
        },
      },
      {
        str: "some str",
        num: 6,
        inner: {
          arr: ["a", "b", "c"],
          num: 7,
        },
      },
      1,
    ]);
  });

  it("collectionSugar stream", async () => {
    let result = await streamSugarCall();
    expect(result).toEqual([
      [1, 2, 3],
      [4, 5, 6],
    ]);
  });

  it("update bug collectionSugar option", async () => {
    let result = await optionSugarCall();
    expect(result).toEqual([[1], ["some"], []]);
  });

  it("math.aqua test 1", async () => {
    let res = await mathTest1Call();

    expect(res).toEqual(-10);
  });

  it("math.aqua test 2", async () => {
    let res = await mathTest2Call();

    expect(res).toEqual(3);
  });

  it("math.aqua test I16", async () => {
    let res = await mathTestI16Call(relay1.peerId);

    expect(res).toEqual([-32, -64, -8, -8]);
  });

  it("math.aqua test I32", async () => {
    let res = await mathTestI32Call(relay1.peerId);

    expect(res).toEqual([-16, -256, -8, 16]);
  });

  it("math.aqua test I64", async () => {
    let res = await mathTestI64Call(relay1.peerId);

    expect(res).toEqual([0, -512, 0, 72]);
  });

  it("math.aqua test U64", async () => {
    let res = await mathTestU64Call(relay1.peerId);

    expect(res).toEqual([96, 4096, 0, -56]);
  });

  it("multiReturn.aqua", async () => {
    let multiReturnResult = await multiReturnCall();
    expect(multiReturnResult).toEqual([
      ["some-str", "random-str", "some-str"],
      5,
      "some-str",
      [1, 2],
      null,
      10,
    ]);
  });

  it("option_gen.aqua", async () => {
    let optionGenResult = await genOptions();
    expect(optionGenResult).toEqual(["none", "some"]);
  });

  it("option_gen.aqua emptyString", async () => {
    let optionGenResult = await genOptionsEmptyString();
    expect(optionGenResult).toEqual(null);
  });

  it("option.aqua", async () => {
    registerHandlers();
    let optionResult = await useOptionalCall();
    let optionalResult = await returnOptionalCall();
    let noneResult = await returnNull();
    expect(optionResult).toBe("hello");
    expect(optionalResult).toBe("optional");
    expect(noneResult).toBe(null);
  });

  it("nestedFuncs.aqua", async () => {
    let nestedFuncsResult = await nestedFuncsCall();
    expect(nestedFuncsResult).toBe("some-str");
  });

  it("nestedData.aqua", async () => {
    let nestedDataResult = await nestedDataCall();
    expect(nestedDataResult).toEqual({
      one: {
        val: "hellohello",
      },
    });
  });

  it("ability.aqua", async () => {
    let result = await abilityCall();
    expect(result).toStrictEqual([
      "declare_const123",
      "efre123",
      "declare_const123",
      12,
    ]);
  });

  it("ability.aqua complex", async () => {
    let result = await complexAbilityCall();
    expect(result).toStrictEqual([false, true]);
  });

  it("ability.aqua ability calls", async () => {
    let result = await checkAbCallsCall();
    expect(result).toStrictEqual([true, false]);
  });

  it("functors.aqua LNG-119 bug", async () => {
    let result = await bugLng119Call();
    expect(result).toEqual([1]);
  });

  it("passArgsCall.aqua", async () => {
    let passArgsResult = await passArgsCall();
    expect(passArgsResult).toBe("client-utilsid");
  });

  it("passArgsCall.aqua bugLNG60", async () => {
    let result = await bugLNG60Call(relayPeerId1);
    expect(result).toBe(true);
  });

  it("streamArgs.aqua", async () => {
    let streamArgsResult = await streamArgsCall();
    expect(streamArgsResult).toEqual([["peer_id", "peer_id"]]);
  });

  it("streamResults.aqua", async () => {
    let streamResultsResult = await streamResultsCall();
    expect(streamResultsResult).toEqual(["new_name", "new_name", "new_name"]);
  });

  it("streamReturn.aqua", async () => {
    let streamReturnResult = await streamReturnCall();
    expect(streamReturnResult).toEqual(["one", "two", "three", "four"]);
  });

  it("streamCapture.aqua simple", async () => {
    let streamCaptureResult = await streamCaptureSimpleCall();
    expect(streamCaptureResult).toEqual(["one", "two", "three"]);
  });

  // TODO: Unskip this after LNG-226 is fixed
  it.skip("streamCapture.aqua return", async () => {
    let streamCaptureResult = await streamCaptureReturnCall();
    expect(streamCaptureResult).toEqual([
      "one",
      "two",
      "three",
      "four",
      "five",
    ]);
  });

  it("assignment.aqua", async () => {
    let assignmentResult = await assignmentCall();
    expect(assignmentResult).toEqual(["abc", "hello"]);
  });

  it("boolAlgebra.aqua", async () => {
    let boolAlgebraResult = await boolAlgebraCall(relayPeerId1);
    expect(boolAlgebraResult).toEqual([
      true,
      true,
      true,
      true,
      false,
      true,
      true,
      false,
      true,
      true,
      false,
      true,
      false,
      true,
      false,
      true,
      false,
      true,
      false,
    ]);
  });

  it("boolAlgebra.aqua compareStreams", async () => {
    let result = await compareStreamsCall(relayPeerId1);
    expect(result).toEqual(true);
  });

  it("boolAlgebra.aqua compareStructs", async () => {
    let result = await compareStructsCall(relayPeerId1, "struct");
    expect(result).toEqual(false);
  });

  it("join.aqua local", async () => {
    let joinLocalCallResult = await joinIdxLocalCall(relayPeerId1);
    expect(joinLocalCallResult.length).toBeGreaterThanOrEqual(2);
  });

  it("stream.aqua", async () => {
    let streamResult = await streamCall();
    expect(streamResult).toEqual([
      "first updated",
      "second updated",
      "third updated",
      "fourth updated",
    ]);
    // bug LNG-84
    let returnNilResult = await returnNilCall();
    expect(returnNilResult).toEqual([]);
    let returnNoneResult = await returnNoneCall();
    expect(returnNoneResult).toBe(null);
  });

  it("stream.aqua nil length", async () => {
    let result = await nilLengthCall();
    expect(result).toEqual(0);
  });

  it("stream.aqua int functor", async () => {
    let streamResult = await streamIntFunctorCall();
    expect(streamResult).toEqual("123");
  });

  it("streamCan.aqua LNG-63", async () => {
    let result = await bugLNG63Call();
    expect(result).toEqual("ok");
  });

  it("streamCan.aqua LNG-63 2", async () => {
    let result = await bugLNG63_2Call();
    expect(result).toEqual(["ok", ["ok"], ["ok", "no", "ok"]]);
  });

  it("streamCan.aqua LNG-63 3", async () => {
    let result = await bugLNG63_3Call();
    expect(result).toEqual(["ok", 1, [1, 3, 2]]);
  });

  it("streamCan.aqua", async () => {
    let streamCanResult = await streamCanCall();
    expect(streamCanResult).toEqual(["a", "b", null]);
  });

  it("streamCallback.aqua", async () => {
    let streamCallResult = await streamCallbackCall();
    expect(streamCallResult).toEqual([]);
  });

  it("literalCall.aqua", async () => {
    let literalCallResult = await literalCall();
    expect(literalCallResult).toBe("some literal");
  });

  it("pushToStream.aqua", async () => {
    let pushToStreamResult = await pushToStreamCall();
    expect(pushToStreamResult).toEqual(["hello", "get_string"]);
  });

  it("declare.aqua", async () => {
    let declareResult = await declareCall();
    expect(declareResult).toBe(
      "small_foodeclare all barsmall_fooexport_constdeclare_constdeclare_const2",
    );
  });

  it("fold.aqua bug #499", async () => {
    let foldCallResult = await foldBug499Call();
    expect(foldCallResult).toEqual([5]);
  });

  it("stream.aqua join", async () => {
    let streamResult = await streamJoinCall();
    expect(streamResult).toEqual("444");
  });

  it("funcs.aqua", async () => {
    let result = await funcsCall();
    expect(result).toEqual([13, 6, 3, 1]);
  }, 7000);

  // it('closures.aqua LNG-58 bug', async () => {
  //     let res = await lng58Bug()
  //     expect(res).toEqual("ok")
  // });

  // TODO: uncomment
  // it('recursiveStreams.aqua', async () => {
  //     let [sucList, loopList] = await recursiveStreamsCall();
  //     console.log(sucList);
  //     console.log(loopList);
  //     expect(loopList).toEqual(['yes', 'yes', 'yes', 'yes', 'no']);
  //     expect(sucList.length).toEqual(5);
  // });

  it("renameVars.aqua", async () => {
    let renameVarsResult = await renameVarsCall();
    expect(renameVarsResult).toEqual(["ok", "ok"]);
  });

  it("callArrow.aqua", async () => {
    let callArrowResult = await callArrowCall(relayPeerId1);

    expect(callArrowResult).toBe("Hello, callArrow call!");
  }, 10000);

  it("fold.aqua", async () => {
    let foldCallResult = await foldCall(relayPeerId1);
    expect(foldCallResult).toEqual(config.externalAddressesRelay1);
  });

  it("par.aqua", async () => {
    let parCallResult = await parCall(relayPeerId1);
    expect(parCallResult).toBe("hello");
  });

  it("par.aqua testTimeout", async () => {
    let testTimeoutResult = await testTimeoutCall();
    expect(testTimeoutResult).toBe("timeout");
  });

  it("canon bug LNG-79", async () => {
    let result = await bugLng79Call(selfPeerId, config.relays[0].peerId);
    expect(result).toBe(2);
  });

  it("on.aqua", async () => {
    let onCallResult = await onCall(relayPeerId1);
    expect(onCallResult).toEqual(config.externalAddressesRelay1);
  });

  it("onErrorPropagate.aqua", async () => {
    let call = onPropagateCall(peer2, relay2.peerId);
    expect(call).rejects.toMatchObject({
      message: expect.stringContaining("propagated error"),
    });
  });

  it("onErrorPropagate.aqua nested", async () => {
    let call = nestedOnPropagateCall(
      peer2,
      relay2.peerId,
      config.relays[3].peerId,
      config.relays[4].peerId,
      config.relays[5].peerId,
    );
    expect(call).rejects.toMatchObject({
      message: expect.stringContaining("propagated error"),
    });
  });

  it("onErrorPropagate.aqua sequential", async () => {
    let call = seqOnPropagateCall(
      peer2,
      relay2.peerId,
      config.relays[3].peerId,
      config.relays[4].peerId,
    );
    expect(call).rejects.toMatchObject({
      message: expect.stringContaining("propagated error"),
    });
  });

  it("errorClear.aqua", async () => {
    let errorClearResult = await errorClearCall(peer2);
    expect(errorClearResult).toEqual(["handle", 0]);
  });

  it("handleResultError.aqua", async () => {
    let call = handleResultErrorCall();

    // js-client return string for interpretation error
    // so matching with object guarantees that error was handled
    expect(call).rejects.toMatchObject({
      message: expect.stringContaining("0"),
      error_code: expect.any(Number),
    });
  });

  it("complex.aqua", async () => {
    let complexCallResult = await complexCall(selfPeerId, relayPeerId1);
    expect(complexCallResult).toEqual([
      "some str",
      "3",
      "1",
      "4",
      "1",
      "1",
      "3",
      "2",
      "4",
      "2",
      "2",
      selfPeerId,
    ]);
  });

  it("object creation getObjRelay", async () => {
    let result = await getObjRelayCall();
    expect(result).toEqual({
      str: "some str",
      num: 5,
      inner: {
        arr: ["a", "b", "c"],
        num: 6,
      },
    });
  });

  it("collectionSugar bug LNG-59", async () => {
    let result = await bugLNG59Call([
      config.relays[2].peerId,
      config.relays[3].peerId,
    ]);
    expect(result).toEqual("some str");
  });

  it("topology.aqua", async () => {
    let topologyResult = await topologyCall(
      peer1,
      relay1.peerId,
      peer2,
      relay2.peerId,
    );
    expect(topologyResult).toBe("finish");
  });

  it("topology.aqua bug 205", async () => {
    let topologyResult = await topologyBug205Call(relay1.peerId, relay2.peerId);
    const peerId2 = relay2.peerId;
    const res: string[] = [peerId2];
    expect(topologyResult).toEqual(res);
  });

  it("topology.aqua bug 427", async () => {
    let topologyResult = await topologyBug427Call(relay1.peerId, relay2.peerId);

    expect(topologyResult).toEqual(["some string", "some string"]);
  });

  it("topology.aqua bug 394", async () => {
    let topologyResult = await topologyBug394Call(
      peer1.getPeerId(),
      relay1.peerId,
      peer2.getPeerId(),
      relay2.peerId,
    );

    expect(topologyResult).toEqual(selfPeerId);
  });

  it("foldJoin.aqua", async () => {
    let foldJoinResult = await foldJoinCall(relayPeerId1);
    expect(foldJoinResult.length).toBeGreaterThanOrEqual(3);
  }, 16000);

  it("via.aqua", async () => {
    let res1 = await viaArrCall();
    let res2 = await viaOptCall(relayPeerId1);
    let res3 = await viaOptNullCall(relayPeerId1);
    let res4 = await viaStreamCall(relayPeerId1);
    expect(res1).toEqual(res2);
    expect(res2).toEqual(res3);
    expect(res3).toEqual(res4);
  }, 180000);

  it("closureReturnRename.aqua bug LNG-193", async () => {
    let result = await lng193BugCall();
    expect(result).toEqual(1 + 42 + (2 + 42) + (3 + 42) + (4 + 42));
  }, 20000);

  it("closures.aqua", async () => {
    let closuresResult = await closuresCall();
    let res1 = config.externalAddressesRelay2;
    let res2 = ["in", config.externalAddressesRelay2[0]];
    expect(closuresResult).toEqual(["in", res1, res1, res2]);
  }, 20000);

  it("closureArrowCapture.aqua", async () => {
    let result = await closureArrowCaptureCall("input");
    expect(result).toEqual("call: ".repeat(4) + "input");
  });

  it("tryOtherwise.aqua", async () => {
    let tryOtherwiseResult = await tryOtherwiseCall(relayPeerId1);
    expect(tryOtherwiseResult).toBe("error");
  }, 20000);

  it("tryCatch.aqua", async () => {
    let tryCatchResult = await tryCatchCall(relayPeerId1);
    expect(tryCatchResult).toHaveLength(2);
    expect(tryCatchResult[0]).toMatch(config.tryCatchError);
    expect(tryCatchResult[1]).toBe(config.externalAddressesRelay1[0]);
  }, 20000);

  it("coCall.aqua", async () => {
    let coCallResult = await coCall();
    expect(coCallResult).toEqual(config.externalAddressesRelay1);
  }, 60000);

  it("join.aqua relay", async () => {
    let joinRelayCallResult = await joinIdxRelayCall(relayPeerId1);
    expect(joinRelayCallResult.length).toBeGreaterThanOrEqual(2);
  }, 30000);

  it("join.aqua network", async () => {
    let joinCallResult = await joinIdxCall(relayPeerId1);
    expect(joinCallResult.length).toBeGreaterThanOrEqual(2);
  }, 10000);
});
