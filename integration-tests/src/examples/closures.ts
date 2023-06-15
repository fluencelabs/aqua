import { Fluence } from '@fluencelabs/fluence';
import {
    closureIn,
    closureOut,
    closureBig,
    registerLocalSrv,
    closureOut2,
    lng58Bug
} from '../compiled/examples/closures.js';
import { config } from '../config.js'

const relays = config.relays

export async function closuresCall(): Promise<[string, string[], string[], [string, string]]> {

    registerLocalSrv({inside: () => console.log("call inside")})

    console.log("closurein")
    const resIn = await closureIn(relays[4].peerId, {ttl: 25000})
    console.log("closureout")
    const resOut = await closureOut(relays[5].peerId, {ttl: 25000})
    console.log("closureout2")
    const resOut2 = await closureOut2(relays[5].peerId, {ttl: 25000})
    console.log("closurebig")
    const resBig = await closureBig(relays[4].peerId, relays[5].peerId, {ttl: 25000})

    return [resIn, resOut.external_addresses, resOut2.external_addresses, resBig]
}

export async function lng58CBugCall(): Promise<string> {
    return lng58Bug()
}