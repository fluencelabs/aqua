import { krasnodar } from "@fluencelabs/fluence-network-environment";
import { FluencePeer } from "@fluencelabs/fluence";
import "helloWorld";

async () => {
  const peer = new FluencePeer();

  peer.init();

  peer.sayHello();

  peer.registerHelloWorld(
    {},
    {
      hello: (arg) => {
        console.log(arg);
      },
    }
  );

  try {
    await peer.sayHello();
    console.log("Said hello");
  } catch (err) {
    console.log("An error occured, ", err);
  }
};
