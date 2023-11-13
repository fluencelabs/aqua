import {
  registerStringer,
  checkStreams,
  returnNilLength,
  returnEmptyLiteral,
  returnStreamFromFunc,
  streamAssignment,
  streamFunctor,
  streamIntFunctor,
  streamJoin,
  stringEmpty,
  stringNone,
} from "../compiled/examples/stream.js";

export async function streamCall() {
  registerStringer({
    returnString: (args0) => {
      return args0 + " updated";
    },
  });

  return checkStreams(["third", "fourth"]);
}

export async function returnNilCall() {
  return stringEmpty();
}

export async function returnNoneCall() {
  return stringNone();
}

export async function streamReturnFromInnerFunc() {
  return await returnStreamFromFunc();
}

export async function streamFunctorCall() {
  return await streamFunctor(["333"]);
}

export async function streamJoinCall() {
  return await streamJoin(["444"]);
}

export async function streamAssignmentCall() {
  return await streamAssignment(["333"]);
}

export async function nilLiteralCall() {
  return await returnEmptyLiteral();
}

export async function nilLengthCall() {
  return await returnNilLength();
}

export async function streamIntFunctorCall() {
  return await streamIntFunctor([0]);
}
