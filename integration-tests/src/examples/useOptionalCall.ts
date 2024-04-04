import {
  returnNone,
  returnOptional,
  useOptional,
  registerSomeS,
  getDefault,
  getArg
} from "../compiled/examples/option.js";

export function registerHandlers(): void {
  registerSomeS({
    getStr: (arg0) => {
      return arg0;
    },
    getStr1: () => {
      return "optional";
    },
    getStr2: (arg0) => {
      return arg0;
    },
    checkU32: (arg) => {},
  });
}

export async function useOptionalCall(): Promise<string> {
  return await useOptional("hello");
}

export async function returnOptionalCall(): Promise<string | null> {
  return await returnOptional();
}

export async function returnNull(): Promise<string | null> {
  return await returnNone();
}

export async function getDefaultCall(): Promise<number> {
  return await getDefault();
}

export async function getArgCall(num: number): Promise<number> {
  return await getArg(num);
}
