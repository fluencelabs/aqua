import { krasnodar } from "@fluencelabs/fluence-network-environment";
import { FluencePeer } from "@fluencelabs/fluence";
import { registerHelloWorld, sayHello } from "./helloWorld";

registerHelloWorld(
  {},
  {
    hello: (arg) => {
      console.log(arg);
    },
  }
);

sayHello()
  .then(() => {
    console.log("Said hello");
  })
  .catch((err) => {
    console.log("An error occured, ", err);
  });
