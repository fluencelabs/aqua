import {getObj, getObjAssign, getObjRelay} from "../compiled/examples/object.js";

export async function getObjCall() {
    return await getObj();
}

export async function getObjRelayCall() {
    return await getObjRelay();
}

export async function getObjAssignCall() {
    return await getObjAssign();
}
