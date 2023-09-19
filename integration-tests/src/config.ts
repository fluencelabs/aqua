import { krasnodar, stage, testNet } from '@fluencelabs/fluence-network-environment';
import { local } from './local-nodes.js';

declare global {
    namespace NodeJS {
        interface ProcessEnv {
            FLUENCE_ENV?: string;
        }
    }
}

function setConfig(env) {
    switch (env) {
        case 'krasnodar':
            return { config: krasnodarConfig, isEphemeral: false };
        case 'testnet':
            return { config: testNetConfig, isEphemeral: false };
        case 'ephemeral':
            return { config: null, isEphemeral: true };
        case 'local':
            return { config: localConfig, isEphemeral: false };
        default:
            return { config: stageConfig, isEphemeral: false };
    }
}

export const krasnodarConfig = {
    relays: krasnodar,
    externalAddressesRelay1: ['/ip4/164.90.171.139/tcp/7770', '/ip4/164.90.171.139/tcp/9990/ws'],
    externalAddressesRelay2: ['/ip4/164.90.164.229/tcp/7001', '/ip4/164.90.164.229/tcp/9001/ws'],
    tryCatchError:
        "Local service error, ret_code is 1, error message is '\"Service with id 'unex' not found (function getStr)\"'",
};

export const stageConfig = {
    relays: stage,
    externalAddressesRelay1: ['/ip4/134.209.186.43/tcp/7001', '/ip4/134.209.186.43/tcp/9001/ws'],
    externalAddressesRelay2: ['/ip4/134.209.186.43/tcp/7770', '/ip4/134.209.186.43/tcp/9990/ws'],
    tryCatchError:
        "Local service error, ret_code is 1, error message is '\"Service with id 'unex' not found (function getStr)\"'",
};

export const testNetConfig = {
    relays: testNet,
    externalAddressesRelay1: ['/ip4/165.227.164.206/tcp/7001', '/ip4/165.227.164.206/tcp/9001/ws'],
    externalAddressesRelay2: ['/ip4/142.93.169.49/tcp/7001', '/ip4/142.93.169.49/tcp/9001/ws'],
    tryCatchError:
        "Local service error, ret_code is 1, error message is '\"Service with id 'unex' not found (function getStr)\"'",
};

// export const ephemeralConfig = {
//   relays: defaultConfig.peers.map((x) => ({
//     peerId: x.peerId,
//     multiaddr: "dontcare",
//   })),
//   externalAddressesRelay1: [],
//   externalAddressesRelay2: [],
//   tryCatchError:
//     "Local service error, ret_code is 1, error message is '\"Service with id 'unex' not found (function getStr)\"'",
// };

export const localConfig = {
    relays: local,
    externalAddressesRelay1: [
        '/ip4/10.50.10.10/tcp/7771',
        '/ip4/10.50.10.10/tcp/9991/ws',
        '/dns4/nox-1/tcp/7771',
        '/dns4/nox-1/tcp/9991/ws',
    ],
    externalAddressesRelay2: [
        '/ip4/10.50.10.60/tcp/7776',
        '/ip4/10.50.10.60/tcp/9996/ws',
        '/dns4/nox-6/tcp/7776',
        '/dns4/nox-6/tcp/9996/ws',
    ],
    tryCatchError:
        "Local service error, ret_code is 1, error message is '\"Service with id 'unex' not found (function getStr)\"'",
};

export const { config, isEphemeral } = setConfig('local');
