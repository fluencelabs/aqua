import { Fluence, IFluenceClient, createClient } from "@fluencelabs/js-client";
import {
  getObjAssignCall,
  getObjCall,
  getObjRelayCall,
  getObjForCall
} from "../examples/objectCall.js";
import {
  callArrowCall,
  reproArgsBug426Call
} from "../examples/callArrowCall.js";
import { dataAliasCall } from "../examples/dataAliasCall.js";
import { onCall } from "../examples/onCall.js";
import {
  testParSeqCall
} from "../examples/parSeqCall.js";
import {
  onPropagateCall,
  nestedOnPropagateCall,
  seqOnPropagateCall
} from "../examples/onErrorPropagation.js";
import { errorClearCall } from "../examples/errorClear.js";
import { handleResultErrorCall } from "../examples/handleResultError.js";
import { funcCall } from "../examples/funcCall.js";
import { registerPrintln } from "../compiled/examples/println.js";
import { helloWorldCall } from "../examples/helloWorldCall.js";
import { foldBug499Call, foldCall } from "../examples/foldCall.js";
import { bugNG69Call, ifCall, ifWrapCall } from "../examples/ifCall.js";
import { simpleStreamScopeCall, complexStreamScopeCall } from "../examples/closureStreamScopesCall.js";
import { ifPropagateErrorsCall } from "../examples/ifPropagateErrors.js";
import { parCall, testTimeoutCall } from "../examples/parCall.js";
import { complexCall } from "../examples/complex.js";
import {
  constantsCall,
  particleTtlAndTimestampCall
} from "../examples/constantsCall.js";
import {
  abilityCall,
  complexAbilityCall,
  checkAbCallsCall,
  bugLNG258Call1,
  bugLNG258Call2,
  bugLNG258Call3,
  multipleAbilityWithClosureCall,
  returnSrvAsAbilityCall
} from "../examples/abilityCall.js";
import { bugLNG314Call, bugLNG338Call } from "../examples/abilityClosureCall.js";
import { bugLNG346Call } from "../examples/abilityClosureRenameCall.js";
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
  streamReturnFromInnerFunc
} from "../examples/streamCall.js";
import {
  testGetFuncCall,
  testGetStreamFuncCall,
  testKeysFuncCall,
  testKeysStreamFuncCall,
  testContainsFuncCall,
  testForFuncCall,
  testForTupleFuncCall,
  testParSeqMapCall
} from "../examples/streamMapCall.js";
import {
  topologyBug205Call,
  topologyBug394Call,
  topologyBug427Call,
  topologyBug257Call,
  topologyCall
} from "../examples/topologyCall.js";
import { foldJoinCall } from "../examples/foldJoinCall.js";
import {
  registerHandlers,
  returnNull,
  returnOptionalCall,
  useOptionalCall,
  getDefaultCall,
  getArgCall
} from "../examples/useOptionalCall.js";
import {
  viaArrCall,
  viaOptCall,
  viaOptNullCall,
  viaStreamCall
} from "../examples/viaCall.js";
import { nestedFuncsCall } from "../examples/nestedFuncsCall.js";
import { assignmentCall } from "../examples/assignment.js";
import {
  boolAlgebraCall,
  compareStreamsCall,
  compareStructsCall
} from "../examples/boolAlgebra.js";
import { tryCatchCall } from "../examples/tryCatchCall.js";
import { tryOtherwiseCall } from "../examples/tryOtherwiseCall.js";
import { coCall } from "../examples/coCall.js";
import { bugLNG60Call, passArgsCall } from "../examples/passArgsCall.js";
import {
  lng280BugCall,
  lng280BugWithForAnonStreamCall,
  lng280BugWithForCall,
  streamArgsCall,
  modifyStreamCall,
  returnDerivedStreamCall,
  lng280BugWithForEmptyStreamFuncCall
} from "../examples/streamArgsCall.js";
import { streamResultsCall } from "../examples/streamResultsCall.js";
import { structuralTypingCall } from "../examples/structuralTypingCall.js";
import {
  servicesAsAbilitiesCall,
  expectedServiceResults,
  servicesAsAbilitiesCaptureCall,
  expectedServiceCaptureResults
} from "../examples/servicesAsAbilities.js";
import { streamReturnCall } from "../examples/streamReturn.js";
import {
  streamCaptureSimpleCall,
  streamCaptureReturnCall
} from "../examples/streamCapture.js";
import {
  streamIfCall,
  streamForCall,
  streamTryCall,
  streamComplexCall
} from "../examples/streamScopes.js";
import { pushToStreamCall } from "../examples/pushToStreamCall.js";
import { literalCall } from "../examples/returnLiteralCall.js";
import { multiReturnCall } from "../examples/multiReturnCall.js";
import { declareCall } from "../examples/declareCall.js";
import { genOptions, genOptionsEmptyString } from "../examples/optionsCall.js";
import { lng193BugCall } from "../examples/closureReturnRename.js";
import {
  closuresCall,
  multipleClosuresLNG262BugCall,
  lng317BugCall,
  lng325BugCall,
  lng325BugTwoFuncsCall
} from "../examples/closures.js";
import { closureArrowCaptureCall } from "../examples/closureArrowCapture.js";
import {
  bugLNG63_2Call,
  bugLNG63_3Call,
  bugLNG63Call,
  streamCanCall
} from "../examples/streamCanCall.js";
import { streamCallbackCall } from "../examples/streamCallback.js";
import { streamResCall } from "../examples/streamRestrictionsCall.js";
import {
  joinIdxCall,
  joinIdxLocalCall,
  joinIdxRelayCall
} from "../examples/joinCall.js";
import { renameVarsCall } from "../examples/renameVars.js";
import {
  arraySugarCall,
  bugLNG59Call,
  optionSugarCall,
  streamSugarCall
} from "../examples/collectionSugarCall.js";
import { funcsCall, bugLNG260Call } from "../examples/funcsCall.js";
import { nestedDataCall } from "../examples/nestedDataCall.js";
import {
  mathTest1Call,
  mathTest2Call,
  mathTestI16Call,
  mathTestI32Call,
  mathTestI64Call,
  mathTestU64Call
} from "../examples/mathCall.js";
import { lng58Bug } from "../compiled/examples/closures.js";
import { config, isEphemeral } from "../config.js";
import { bugLng79Call } from "../examples/canonCall.js";
import { bugLng119Call } from "../examples/functorsCall.js";
import {
  returnArrowCall,
  returnArrowChainCall
} from "../examples/returnArrowCall.js";
import { rangeCall } from "../examples/recursiveStreams/rangeCall.js";
import { nestedCall } from "../examples/recursiveStreams/nestedCall.js";
import { yesNoStreamCall } from "../examples/recursiveStreams/yesNoStreamCall.js";
import { multiRecStreamCall } from "../examples/recursiveStreams/multiRecStreamCall.js";
import { pipelineStreamCall } from "../examples/recursiveStreams/pipelineCall.js";
import { remoteRecStreamCall } from "../examples/recursiveStreams/remoteRecCall.js";

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
  await Fluence.connect(relay1, {});

  peer1 = Fluence.getClient();
  selfPeerId = peer1.getPeerId();

  peer2 = await createClient(relay2);
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
      }
    });
  }, 12000);

  afterAll(async () => {
    await stop();
  });

  describe("for ... rec", () => {
    const range = (start: number, end: number) =>
      Array.from({ length: end - start }, (v, k) => k + start);

    it("range", async () => {
      for (const i of range(-5, 5)) {
        for (const j of range(-5, 5)) {
          const result = await rangeCall(i, j);
          if (i < j) {
            expect(result).toEqual(range(i, j));
          } else {
            expect(result).toEqual([]);
          }
        }
      }
    }, 15000);

    /**
     * This test does not work due to Aqua VM
     */
    it.skip("nested", async () => {
      for (const i of range(0, 10)) {
        const result = await nestedCall(i);
        expect(result).toEqual(range(0, i).flatMap((x) => range(0, x + 1)));
      }
    }, 15000);

    it("yes|no stream", async () => {
      for (const i of range(1, 10)) {
        const yesNo = await yesNoStreamCall(i);
        expect(yesNo).toEqual(
          range(0, i)
            .map((_) => "yes")
            .concat(["no"])
        );
      }
    }, 15000);

    it("multi rec stream", async () => {
      const handle = (i: number) => {
        if (i % 3 === 0) return [i + 1];
        if (i % 3 === 1) return [i + 1, i + 2];
        return [];
      };
      for (const i of range(1, 10)) {
        const loop = await multiRecStreamCall(0, i, handle);
        range(0, i + 1).forEach((j) => {
          expect(loop).toContain(j);
        });
      }
    }, 15000);

    it("pipeline", async () => {
      for (const i of range(1, 10)) {
        const result = await pipelineStreamCall(0, i);
        expect(result.sort()).toEqual(range(0, i + 1));
      }
    }, 15000);

    /**
     * This test does not work due to `for ... rec`
     * not taking topology into account
     */
    it.skip("remote rec", async () => {
      for (const i of range(0, 10)) {
        const result = await remoteRecStreamCall(0, i, peer2);
        expect(result).toEqual(range(0, i + 1));
      }
    }, 15000);
  });

  it("callArrow.aqua args bug 426", async () => {
    const argResult = await reproArgsBug426Call();

    expect(argResult).toBe("privet");
  });

  it("returnArrow.aqua", async () => {
    const [result1, result2] = await returnArrowCall();

    expect(result1).toBe("arg for closure ");
    expect(result2).toBe("arg for closure arg for func  literal");
  });

  it("returnArrow.aqua chain", async () => {
    const argResult = await returnArrowChainCall();

    expect(argResult).toStrictEqual([
      "first",
      "firstarg for func1  literal",
      "second",
      "secondarg for func2  second literal",
      "third",
      "thirdarg for func2  second literal",
      "fourth",
      "fourth from second literal"
    ]);
  });

  it("streamRestrictions.aqua", async () => {
    const streamResResult = await streamResCall();

    expect(streamResResult).toEqual([[], ["a", "b", "c"]]);
  });

  it("streamScopes.aqua streamIf", async () => {
    const streamIfResult = await streamIfCall();

    expect(streamIfResult).toEqual(5);
  });

  it("streamScopes.aqua streamTry", async () => {
    const streamTryResult = await streamTryCall();

    expect(streamTryResult).toEqual(4);
  });

  it("streamScopes.aqua streamFor", async () => {
    const streamTryResult = await streamForCall();

    expect(streamTryResult).toEqual(4);
  });

  it("streamScopes.aqua streamComplex", async () => {
    const streamTryResult = await streamComplexCall();

    expect(streamTryResult).toEqual(13);
  });

  it("if.aqua", async () => {
    await ifCall();
  });

  it("if.aqua xor wrap", async () => {
    const res = await ifWrapCall(relay2.peerId);
    expect(res).toBe("1x");
  });

  it("if.aqua bug LNG-69", async () => {
    const res = await bugNG69Call(relay2.peerId);
    expect(res).toBe(true);
  });

  it("ifPropagateErrors.aqua", async () => {
    const res = await ifPropagateErrorsCall();
    expect(res).toEqual([1, 2, 3].map((i) => "otherwise" + i));
  });

  it("helloWorld.aqua", async () => {
    const helloWorldResult = await helloWorldCall();
    expect(helloWorldResult).toBe("Hello, NAME!");
  });

  it("func.aqua", async () => {
    const funcCallResult = await funcCall();
    expect(funcCallResult).toBe("some str");
  });

  it("dataAlias.aqua", async () => {
    const dataAliasResult = await dataAliasCall();
    expect(dataAliasResult).toBe("peer id str");
  });

  it("constants.aqua", async () => {
    const constantCallResult = await constantsCall();
    expect(constantCallResult).toEqual(["5", "default-str"]);
  });

  it("PARTICLE_TTL and PARTICLE_TIMESTAMP", async () => {
    const ttl = 1234;
    const result = await particleTtlAndTimestampCall(ttl);
    expect(result[1]).toBeDefined();
    expect(result[0]).toEqual(ttl);
  });

  it("stream.aqua return stream from inner func", async () => {
    const streamResult = await streamReturnFromInnerFunc();
    expect(streamResult).toEqual([1, 2, 3, 4]);
  });

  it("stream.aqua functor", async () => {
    const streamResult = await streamFunctorCall();
    expect(streamResult).toEqual("123");
  });

  it("stream.aqua assignment", async () => {
    const streamResult = await streamAssignmentCall();
    expect(streamResult).toEqual("333");
  });

  it("stream.aqua nil literal", async () => {
    const result = await nilLiteralCall();
    expect(result).toEqual([]);
  });

  it("structuraltyping.aqua", async () => {
    const result = await structuralTypingCall();
    expect(result).toEqual("some_stringsome_stringsome_stringab_string");
  });

  it("servicesAsAbilities.aqua", async () => {
    const result = await servicesAsAbilitiesCall();
    expect(result).toEqual(expectedServiceResults);
  });

  it("servicesAsAbilities.aqua capture", async () => {
    const result = await servicesAsAbilitiesCaptureCall();
    expect(result).toEqual(expectedServiceCaptureResults);
  });

  it("collectionSugar array", async () => {
    const result = await arraySugarCall();
    expect(result).toEqual([
      [1, 2, 3],
      [4, 5, 6]
    ]);
  });

  it("object creation getObj", async () => {
    const result = await getObjCall();
    expect(result).toEqual({
      str: "some str",
      num: 5,
      inner: {
        arr: ["a", "b", "c"],
        num: 6
      }
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
          num: 11
        }
      },
      {
        str: "second copied",
        num: 2,
        inner: {
          arr: ["copy"],
          num: 22
        }
      },
      {
        str: "third copied",
        num: 3,
        inner: {
          arr: ["copy"],
          num: 33
        }
      },
      {
        str: "for",
        num: 1,
        inner: {
          arr: [],
          num: 1
        }
      },
      {
        str: "for",
        num: 2,
        inner: {
          arr: [],
          num: 2
        }
      },
      {
        str: "for",
        num: 3,
        inner: {
          arr: [],
          num: 3
        }
      }
    ];
    expect(result).toEqual(res);
  });

  it("object creation getObjAssign", async () => {
    const result = await getObjAssignCall();
    expect(result).toEqual([
      {
        str: "first str",
        num: 5,
        inner: {
          arr: ["d", "e", "f"],
          num: 7
        }
      },
      {
        str: "some str",
        num: 6,
        inner: {
          arr: ["a", "b", "c"],
          num: 7
        }
      },
      1
    ]);
  });

  it("collectionSugar stream", async () => {
    const result = await streamSugarCall();
    expect(result).toEqual([
      [1, 2, 3],
      [4, 5, 6]
    ]);
  });

  it("update bug collectionSugar option", async () => {
    const result = await optionSugarCall();
    expect(result).toEqual([[1], ["some"], []]);
  });

  it("math.aqua test 1", async () => {
    const res = await mathTest1Call();

    expect(res).toEqual(-10);
  });

  it("math.aqua test 2", async () => {
    const res = await mathTest2Call();

    expect(res).toEqual(3);
  });

  it("math.aqua test I16", async () => {
    const res = await mathTestI16Call(relay1.peerId);

    expect(res).toEqual([-32, -64, -8, -8]);
  });

  it("math.aqua test I32", async () => {
    const res = await mathTestI32Call(relay1.peerId);

    expect(res).toEqual([-16, -256, -8, 16]);
  });

  it("math.aqua test I64", async () => {
    const res = await mathTestI64Call(relay1.peerId);

    expect(res).toEqual([0, -512, 0, 72]);
  });

  it("math.aqua test U64", async () => {
    const res = await mathTestU64Call(relay1.peerId);

    expect(res).toEqual([96, 4096, 0, -56]);
  });

  it("multiReturn.aqua", async () => {
    const multiReturnResult = await multiReturnCall();
    expect(multiReturnResult).toEqual([
      ["some-str", "random-str", "some-str"],
      5,
      "some-str",
      [1, 2],
      null,
      10
    ]);
  });

  it("option_gen.aqua", async () => {
    const optionGenResult = await genOptions();
    expect(optionGenResult).toEqual(["none", "some"]);
  });

  it("option_gen.aqua emptyString", async () => {
    const optionGenResult = await genOptionsEmptyString();
    expect(optionGenResult).toEqual(null);
  });

  it("option.aqua", async () => {
    registerHandlers();
    const optionResult = await useOptionalCall();
    const optionalResult = await returnOptionalCall();
    const noneResult = await returnNull();
    expect(optionResult).toBe("hello");
    expect(optionalResult).toBe("optional");
    expect(noneResult).toBe(null);
  });

  it("option.aqua default values LNG-351", async () => {
    registerHandlers();
    const defaultResult = await getDefaultCall();
    const argResult = await getArgCall(1000);
    expect(defaultResult).toBe(42);
    expect(argResult).toBe(100);
  });

  it("nestedFuncs.aqua", async () => {
    const nestedFuncsResult = await nestedFuncsCall();
    expect(nestedFuncsResult).toBe("some-str");
  });

  it("nestedData.aqua", async () => {
    const nestedDataResult = await nestedDataCall();
    expect(nestedDataResult).toEqual({
      one: {
        val: "hellohello"
      }
    });
  });

  it("abilities.aqua", async () => {
    const result = await abilityCall();
    expect(result).toStrictEqual([
      "declare_const123",
      "efre123",
      "declare_const123",
      12
    ]);
  });

  it("abilities.aqua complex", async () => {
    const result = await complexAbilityCall();
    expect(result).toStrictEqual([false, true]);
  });

  it("abilities.aqua ability calls", async () => {
    const result = await checkAbCallsCall();
    expect(result).toStrictEqual([true, false, true]);
  });

  it("abilities.aqua bug LNG-258", async () => {
    const result1 = await bugLNG258Call1();
    expect(result1).toStrictEqual([1, 2]);

    const result2 = await bugLNG258Call2();
    expect(result2).toStrictEqual([3, 4]);

    const result3 = await bugLNG258Call3();
    expect(result3).toStrictEqual([5, 6]);
  });

  it("abilities.aqua multiple abilities with closures", async () => {
    const result1 = await multipleAbilityWithClosureCall();
    expect(result1).toStrictEqual([1, 2]);
  });

  it("abilities.aqua return service as ability", async () => {
    const result = await returnSrvAsAbilityCall();
    expect(result).toStrictEqual(["default-id", "resolved-id"]);
  });

  it("abilitiesClosure.aqua bug LNG-314", async () => {
    const result = await bugLNG314Call();
    expect(result).toEqual("strstrstr");
  });

  it("abilitiesClosure.aqua bug LNG-338", async () => {
    const result = await bugLNG338Call();
    expect(result).toEqual("job done");
  });

  it("abilitiesClosureRename.aqua bug LNG-346", async () => {
    const result = await bugLNG346Call();
    expect(result).toEqual("hello");
  });

  it("functors.aqua LNG-119 bug", async () => {
    const result = await bugLng119Call();
    expect(result).toEqual([1]);
  });

  it("passArgsCall.aqua", async () => {
    const passArgsResult = await passArgsCall();
    expect(passArgsResult).toBe("client-utilsid");
  });

  it("passArgsCall.aqua bugLNG60", async () => {
    const result = await bugLNG60Call(relayPeerId1);
    expect(result).toBe(true);
  });

  it("streamArgs.aqua", async () => {
    const streamArgsResult = await streamArgsCall();
    expect(streamArgsResult).toEqual([["peer_id", "peer_id"]]);
  });

  it("streamArgs.aqua modify stream", async () => {
    const streamArgsResult = await modifyStreamCall([
      "passed value 1",
      "passed value 2"
    ]);
    expect(streamArgsResult).toEqual([
      "passed value 1",
      "passed value 2",
      "appended value"
    ]);
  });

  it.skip("streamArgs.aqua LNG-280", async () => {
    const result = await lng280BugCall();
    expect(result).toEqual(["valueUseStream", "valueReturnStream", "valueTop"]);
  });

  it.skip("streamArgs.aqua LNG-280 with for", async () => {
    const result = await lng280BugWithForCall();
    expect(result).toEqual([
      "valueUseStream",
      "valueReturnStream",
      "valueUseStream",
      "valueReturnStream",
      "valueUseStream",
      "valueReturnStream"
    ]);
  });

  it("streamArgs.aqua LNG-280 with for and anonymous stream", async () => {
    const result = await lng280BugWithForAnonStreamCall();
    expect(result).toEqual([
      [1, 1],
      [1, 2],
      [1, 3],
      [1, 4],
      [1, 5]
    ]);
  });

  it("streamArgs.aqua LNG-280 with for and anonymous stream from function", async () => {
    const result = await lng280BugWithForEmptyStreamFuncCall();
    expect(result).toEqual([
      [1, 1],
      [1, 2],
      [1, 3],
      [1, 4],
      [1, 5]
    ]);
  });

  it.skip("streamArgs.aqua return derived stream", async () => {
    const result = await returnDerivedStreamCall();
    expect(result).toEqual([1]);
  });

  it("streamResults.aqua", async () => {
    const streamResultsResult = await streamResultsCall();
    expect(streamResultsResult).toEqual(["new_name", "new_name", "new_name"]);
  });

  it("streamReturn.aqua", async () => {
    const streamReturnResult = await streamReturnCall();
    expect(streamReturnResult).toEqual(["one", "two", "three", "four"]);
  });

  it("streamCapture.aqua simple", async () => {
    const streamCaptureResult = await streamCaptureSimpleCall();
    expect(streamCaptureResult).toEqual(["one", "two", "three"]);
  });

  it("closureStreamScopes.aqua simple", async () => {
    const result = await simpleStreamScopeCall();
    // it is not hanging
    expect(result).toEqual(["result"]);
  });

  it("closureStreamScopes.aqua complex", async () => {
    const result = await complexStreamScopeCall();
    expect(result).toEqual([["something in INSIDE", "something out INSIDE"], ["something out OUTSIDE"]]);
  });

  // TODO: Unskip this after LNG-226 is fixed
  it.skip("streamCapture.aqua return", async () => {
    const streamCaptureResult = await streamCaptureReturnCall();
    expect(streamCaptureResult).toEqual([
      "one",
      "two",
      "three",
      "four",
      "five"
    ]);
  });

  it("assignment.aqua", async () => {
    const assignmentResult = await assignmentCall();
    expect(assignmentResult).toEqual(["abc", "hello"]);
  });

  it("boolAlgebra.aqua", async () => {
    const boolAlgebraResult = await boolAlgebraCall(relayPeerId1);
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
      false
    ]);
  });

  it("boolAlgebra.aqua compareStreams", async () => {
    const result = await compareStreamsCall(relayPeerId1);
    expect(result).toEqual(true);
  });

  it("boolAlgebra.aqua compareStructs", async () => {
    const result = await compareStructsCall(relayPeerId1, "struct");
    expect(result).toEqual(false);
  });

  it("join.aqua local", async () => {
    const joinLocalCallResult = await joinIdxLocalCall(relayPeerId1);
    expect(joinLocalCallResult.length).toBeGreaterThanOrEqual(2);
  });

  it("streamMap.aqua get function call", async () => {
    const [resEmpty, resFirst, resSecond, length] = await testGetFuncCall();
    expect(resEmpty).toEqual([]);
    expect(resFirst).toEqual(["first value"]);
    expect(resSecond).toEqual(["first value", "second value"]);
    expect(length).toEqual(2);
  });

  it("streamMap.aqua get stream function call", async () => {
    const [resEmpty, resFirst, resSecond] = await testGetStreamFuncCall();
    expect(resEmpty).toEqual([]);
    expect(resFirst).toEqual("first value");
    expect(resSecond).toEqual("second value");
  });

  it("streamMap.aqua keys function call", async () => {
    const [resEmpty, resFirst, resSecond] = await testKeysFuncCall();
    expect(resEmpty).toEqual([]);
    expect(resFirst).toEqual(["key one"]);
    // FIX: it shouldn't work this way, it should return unique keys
    expect(resSecond).toEqual(["key one", "key two", "key one", "key one"]);
  });

  it("streamMap.aqua keysStream function call", async () => {
    const [resEmpty, resFirst, resSecond] = await testKeysStreamFuncCall();
    expect(resEmpty).toEqual([]);
    expect(resFirst).toEqual(["key one"]);
    expect(resSecond).toEqual(["key one", "key one", "key two"]);
  });

  it("streamMap.aqua contains function call", async () => {
    const res = await testContainsFuncCall();
    expect(res).toEqual([false, true, false, true, true]);
  });

  it("streamMap.aqua call with for over map", async () => {
    const [keys, values] = await testForFuncCall();
    expect(keys).toEqual(["key one", "key one", "key two", "key two", "key two", "key three", "key four"]);
    expect(values).toEqual(["1", "2", "3", "4", "5", "6", "7"]);
  });

  it("streamMap.aqua call with for with tuples over map", async () => {
    const [arr1, arr2] = await testForTupleFuncCall();
    console.log("arr1")
    console.log(arr1)
    console.log("arr2")
    console.log(arr2)
    expect(arr1).toEqual(["key one", "key one", "key two", "key two", "key two", "key three", "key four",
      "1", "2", "3", "4", "5", "6", "7",]);
    expect(arr2).toEqual(["1", "2", "3", "4", "5", "6", "7",
      "key one", "key one", "key two", "key two", "key two", "key three", "key four"]);
  });

  it("streamMap.aqua call with parseq over map", async () => {
    const res = await testParSeqCall();
    expect(res).toEqual("ok");
  });

  it("stream.aqua", async () => {
    const streamResult = await streamCall();
    expect(streamResult).toEqual([
      "first updated",
      "second updated",
      "third updated",
      "fourth updated"
    ]);
    // bug LNG-84
    const returnNilResult = await returnNilCall();
    expect(returnNilResult).toEqual([]);
    const returnNoneResult = await returnNoneCall();
    expect(returnNoneResult).toBe(null);
  });

  it("stream.aqua nil length", async () => {
    const result = await nilLengthCall();
    expect(result).toEqual(0);
  });

  it("stream.aqua int functor", async () => {
    const streamResult = await streamIntFunctorCall();
    expect(streamResult).toEqual("123");
  });

  it("streamCan.aqua LNG-63", async () => {
    const result = await bugLNG63Call();
    expect(result).toEqual("ok");
  });

  it("streamCan.aqua LNG-63 2", async () => {
    const result = await bugLNG63_2Call();
    expect(result).toEqual(["ok", ["ok"], ["ok", "no", "ok"]]);
  });

  it("streamCan.aqua LNG-63 3", async () => {
    const result = await bugLNG63_3Call();
    expect(result).toEqual(["ok", 1, [1, 3, 2]]);
  });

  it("streamCan.aqua", async () => {
    const streamCanResult = await streamCanCall();
    expect(streamCanResult).toEqual(["a", "b", null]);
  });

  it("parseq.aqua", async () => {
    const res = await testParSeqCall();
    expect(res).toEqual("ok");
  });

  it("streamCallback.aqua", async () => {
    const streamCallResult = await streamCallbackCall();
    expect(streamCallResult).toEqual([]);
  });

  it("literalCall.aqua", async () => {
    const literalCallResult = await literalCall();
    expect(literalCallResult).toBe("some literal");
  });

  it("pushToStream.aqua", async () => {
    const pushToStreamResult = await pushToStreamCall();
    expect(pushToStreamResult).toEqual(["hello", "get_string"]);
  });

  it("declare.aqua", async () => {
    const declareResult = await declareCall();
    expect(declareResult).toBe(
      "small_foodeclare all barsmall_fooexport_constdeclare_constdeclare_const2"
    );
  });

  it("fold.aqua bug #499", async () => {
    const foldCallResult = await foldBug499Call();
    expect(foldCallResult).toEqual([5]);
  });

  it("stream.aqua join", async () => {
    const streamResult = await streamJoinCall();
    expect(streamResult).toEqual("444");
  });

  it("funcs.aqua", async () => {
    const result = await funcsCall();
    expect(result).toEqual([13, 6, 3, 1]);
  }, 7000);

  it("funcs.aqua bugLNG260", async () => {
    const result1 = await bugLNG260Call(1, 2);
    expect(result1).toEqual(false);
    const result2 = await bugLNG260Call(4, 3);
    expect(result2).toEqual(true);
    const result3 = await bugLNG260Call(5, 5);
    expect(result3).toEqual(false);
  });

  // it('closures.aqua LNG-58 bug', async () => {
  //     const res = await lng58Bug()
  //     expect(res).toEqual("ok")
  // });

  it("renameVars.aqua", async () => {
    const renameVarsResult = await renameVarsCall();
    expect(renameVarsResult).toEqual(["ok", "ok"]);
  });

  it("callArrow.aqua", async () => {
    const callArrowResult = await callArrowCall(relayPeerId1);

    expect(callArrowResult).toBe("Hello, callArrow call!");
  }, 10000);

  it("fold.aqua", async () => {
    const foldCallResult = await foldCall(relayPeerId1);
    expect(foldCallResult).toEqual(config.externalAddressesRelay1);
  });

  it("par.aqua", async () => {
    const parCallResult = await parCall(relayPeerId1);
    expect(parCallResult).toBe("hello");
  });

  it("par.aqua testTimeout", async () => {
    const testTimeoutResult = await testTimeoutCall();
    expect(testTimeoutResult).toBe("timeout");
  });

  it("canon bug LNG-79", async () => {
    const result = await bugLng79Call(selfPeerId, config.relays[0].peerId);
    expect(result).toBe(2);
  });

  it("on.aqua", async () => {
    const onCallResult = await onCall(relayPeerId1);
    expect(onCallResult).toEqual(config.externalAddressesRelay1);
  });

  it("onErrorPropagate.aqua", async () => {
    const call = onPropagateCall(peer2, relay2.peerId);
    expect(call).rejects.toMatchObject({
      message: expect.stringContaining("propagated error")
    });
  });

  it("onErrorPropagate.aqua nested", async () => {
    const call = nestedOnPropagateCall(
      peer2,
      relay2.peerId,
      config.relays[3].peerId,
      config.relays[4].peerId,
      config.relays[5].peerId
    );
    expect(call).rejects.toMatchObject({
      message: expect.stringContaining("propagated error")
    });
  });

  it("onErrorPropagate.aqua sequential", async () => {
    const call = seqOnPropagateCall(
      peer2,
      relay2.peerId,
      config.relays[3].peerId,
      config.relays[4].peerId
    );
    expect(call).rejects.toMatchObject({
      message: expect.stringContaining("propagated error")
    });
  });

  it("errorClear.aqua", async () => {
    const errorClearResult = await errorClearCall(peer2);
    expect(errorClearResult).toEqual(["handle", 0]);
  });

  it("handleResultError.aqua", async () => {
    const call = handleResultErrorCall();

    // js-client return string for interpretation error
    // so matching with object guarantees that error was handled
    expect(call).rejects.toMatchObject({
      message: expect.stringContaining("0"),
      error_code: expect.any(Number)
    });
  });

  it("complex.aqua", async () => {
    const complexCallResult = await complexCall(selfPeerId, relayPeerId1);
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
      selfPeerId
    ]);
  });

  it("object creation getObjRelay", async () => {
    const result = await getObjRelayCall();
    expect(result).toEqual({
      str: "some str",
      num: 5,
      inner: {
        arr: ["a", "b", "c"],
        num: 6
      }
    });
  });

  it("collectionSugar bug LNG-59", async () => {
    const result = await bugLNG59Call([
      config.relays[2].peerId,
      config.relays[3].peerId
    ]);
    expect(result).toEqual("some str");
  });

  it("topology.aqua", async () => {
    const topologyResult = await topologyCall(
      peer1,
      relay1.peerId,
      peer2,
      relay2.peerId
    );
    expect(topologyResult).toBe("finish");
  });

  it("topology.aqua bug 205", async () => {
    const topologyResult = await topologyBug205Call(relay1.peerId, relay2.peerId);
    const peerId2 = relay2.peerId;
    const res: string[] = [peerId2];
    expect(topologyResult).toEqual(res);
  });

  it("topology.aqua bug 427", async () => {
    const topologyResult = await topologyBug427Call(relay1.peerId, relay2.peerId);

    expect(topologyResult).toEqual(["some string", "some string"]);
  });

  it("topology.aqua bug 394", async () => {
    const topologyResult = await topologyBug394Call(
      peer1.getPeerId(),
      relay1.peerId,
      peer2.getPeerId(),
      relay2.peerId
    );

    expect(topologyResult).toEqual(selfPeerId);
  });

  it("topology.aqua bug 257", async () => {
    const result = await topologyBug257Call(peer2);
    expect(result).toEqual(["host", "friend", "init"]);
  });

  it("foldJoin.aqua", async () => {
    const foldJoinResult = await foldJoinCall(relayPeerId1);
    expect(foldJoinResult.length).toBeGreaterThanOrEqual(3);
  }, 16000);

  it("via.aqua", async () => {
    const res1 = await viaArrCall();
    const res2 = await viaOptCall();
    const res3 = await viaOptNullCall();
    const res4 = await viaStreamCall();
    expect(res1).not.toHaveLength(0);
    expect(res1).toEqual(res2);
    expect(res2).toEqual(res3);
    expect(res3).toEqual(res4);
  }, 180000);

  it("closureReturnRename.aqua bug LNG-193", async () => {
    const result = await lng193BugCall();
    expect(result).toEqual(1 + 42 + (2 + 42) + (3 + 42) + (4 + 42));
  }, 20000);

  it("closures.aqua", async () => {
    const closuresResult = await closuresCall();
    const res1 = config.externalAddressesRelay2;
    const res2 = ["in", config.externalAddressesRelay2[0]];
    expect(closuresResult).toEqual(["in", res1, res1, res2]);
  }, 20000);

  it("closures.aqua bug LNG-262", async () => {
    const result = await multipleClosuresLNG262BugCall();
    expect(result).toEqual([1, 2]);
  });

  it("closures.aqua bug LNG-317", async () => {
    const result = await lng317BugCall();
    expect(result).toEqual(["empty", "identity"]);
  });

  it("closures.aqua bug LNG-325", async () => {
    const result = await lng325BugCall();
    expect(result).toEqual("firstStream string");
  });

  it("closures.aqua bug LNG-325 two functions", async () => {
    const result = await lng325BugTwoFuncsCall();
    expect(result).toEqual(["firstStream string", "secondStream string"]);
  });

  it("closureArrowCapture.aqua", async () => {
    const result = await closureArrowCaptureCall("input");
    expect(result).toEqual("call: ".repeat(4) + "input");
  });

  it("tryOtherwise.aqua", async () => {
    const tryOtherwiseResult = await tryOtherwiseCall(relayPeerId1);
    expect(tryOtherwiseResult).toBe("error");
  }, 20000);

  it("tryCatch.aqua", async () => {
    const tryCatchResult = await tryCatchCall(relayPeerId1);
    expect(tryCatchResult).toHaveLength(2);
    expect(tryCatchResult[0]).toMatch(config.tryCatchError);
    expect(tryCatchResult[1]).toBe(config.externalAddressesRelay1[0]);
  }, 20000);

  it("coCall.aqua", async () => {
    const coCallResult = await coCall();
    expect(coCallResult).toEqual(config.externalAddressesRelay1);
  }, 60000);

  it("join.aqua relay", async () => {
    const joinRelayCallResult = await joinIdxRelayCall(relayPeerId1);
    expect(joinRelayCallResult.length).toBeGreaterThanOrEqual(2);
  }, 30000);

  it("join.aqua network", async () => {
    const joinCallResult = await joinIdxCall(relayPeerId1);
    expect(joinCallResult.length).toBeGreaterThanOrEqual(2);
  }, 10000);
});
