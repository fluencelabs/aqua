#!/usr/bin/env node

import { Fluence, FluencePeer } from '@fluencelabs/fluence';
import { krasnodar } from '@fluencelabs/fluence-network-environment';
import { helloWorld, registerStringExtra } from './compiled/examples/helloWorld.js';

const main = async () => {
    // each compiled aqua function require a connected client
    await Fluence.start({ connectTo: krasnodar[0] });

    // example to how register a local service
    // it could be used in aqua code as follows
    // service StringExtra("service-id"):
    //     addNameToHello: string -> string
    // see more in helloWorld.aqua
    registerStringExtra({
        addNameToHello: (arg0) => {
            return `Hello, ${arg0}!`;
        },
    });

    // call an aqua function thet presented in ../aqua/helloWorld.aqua
    const result = await helloWorld('NAME');
    console.log(result);

    // uncomment to play with examples
    // await runExamples();

    process.exit(0);
};

main().catch((err) => {
    console.log(err);
    process.exit(1);
});
