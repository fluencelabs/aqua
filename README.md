## Aquamarine HLL

The high level language that compiles to AIR and some wrappers in the host language (e.g. TypeScript).

## Build and use jar file
To build a file use this command:

```commandline
sbt assembly
```


jar file will be in `target/scala-2.13/aqua-hll.jar`

Run:

```commandline
java -jar aqua-hll.jar path/to/input/dir path/to/output/dir
```
input directory should contain files with `aqua` scripts

### TODO

- Lambda
- Build data types: extend
- Platform-specific Predef with Return ability
- Implementation for abilities
- Abilities passing
- Print syntax errors better way
- For the offset, find a token

### Wishlist

1)
if aaa == bbb: -- !=

else:

2)
try:

catch err:

3)
$streams

4...)
while x:
backpressure...

5)
use "smth.aqua" as Smth

6)
ability passing:
func x {Ab} -> y
func x {Ab} -> y {Foo}
func x -> y {Foo}

7)
on Ability x:

8)
on browserId via relayId:

9)
compilation targets
- ts
- pure air
- ???????????????????

10)
use blueprint "blueprint id" as ServiceName

11)
kademlia in Aqua
onKadNeighborhood(kad_key, replicationFactor) do:
....

12)
better lambda expressions

13)
@-expression

alias AuthToken: bool

func foo(token: AuthToken @check[from app.auth])

14) FUTURE
    generate facade modules from Aqua