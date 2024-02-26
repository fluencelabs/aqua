import {
  bugLNG314, bugLNG338
} from "../compiled/examples/abilitiesClosure.js";

export async function bugLNG314Call(): Promise<string> {
  return await bugLNG314();
}

export async function bugLNG338Call(): Promise<string> {
  return await bugLNG338();
}
