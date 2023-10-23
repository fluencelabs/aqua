import {
  handleAb,
  registerSomeService,
  bug214,
  checkAbCalls,
  bugLNG258_1,
  bugLNG258_2,
  bugLNG258_3,
} from "../compiled/examples/abilities";

export async function abilityCall(): Promise<[string, string, string, number]> {
  registerSomeService({
    getStr: (s: string) => {
      return s + "123";
    },
  });

  return await handleAb("some_string");
}

export async function complexAbilityCall(): Promise<[boolean, boolean]> {
  return await bug214();
}

export async function checkAbCallsCall(): Promise<[boolean, boolean, boolean]> {
  return await checkAbCalls();
}

export async function bugLNG258Call1(): Promise<[number, number]> {
  return await bugLNG258_1();
}

export async function bugLNG258Call2(): Promise<[number, number]> {
  return await bugLNG258_2();
}

export async function bugLNG258Call3(): Promise<[number, number]> {
  return await bugLNG258_3();
}
