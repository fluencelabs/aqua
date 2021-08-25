#! /bin/bash

sbt cli/assembly
java -jar cli/.jvm/target/scala-3.0.1/aqua-cli-0.1.13-SNAPSHOT.jar --import . -i ./playground/ts -o ./playground/ts/src