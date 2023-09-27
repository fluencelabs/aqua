import {getObj, getObjAssign, getObjRelay, getObjFor} from "../compiled/examples/object.js";

export async function getObjCall() {
    return await getObj();
}

export async function getObjRelayCall() {
    return await getObjRelay();
}

export async function getObjAssignCall() {
    return await getObjAssign();
}

export async function getObjForCall() {
    return await getObjFor();
}
