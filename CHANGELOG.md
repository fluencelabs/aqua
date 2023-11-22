# Changelog

## [0.12.4](https://github.com/fluencelabs/aqua/compare/aqua-v0.12.3...aqua-v0.12.4) (2023-10-23)


### Features

* **compiler:** Handle error function exit in tracing mode [LNG-250] ([#921](https://github.com/fluencelabs/aqua/issues/921)) ([03d23eb](https://github.com/fluencelabs/aqua/commit/03d23eb577d72a3cf592254259aeb9b52b33b616))
* **compiler:** Optimize math in compile time [LNG-245] ([#922](https://github.com/fluencelabs/aqua/issues/922)) ([5f6c47f](https://github.com/fluencelabs/aqua/commit/5f6c47ffea5ab6e32df918a33414482129b00fd7))
* **dev:** Add PR template ([#934](https://github.com/fluencelabs/aqua/issues/934)) ([679d43f](https://github.com/fluencelabs/aqua/commit/679d43f7eb079071ca2a4174f3b8cf5e83d4e16d))


### Bug Fixes

* **compiler:** Fix gate inlining [LNG-253] ([#924](https://github.com/fluencelabs/aqua/issues/924)) ([b298eeb](https://github.com/fluencelabs/aqua/commit/b298eebf5ea1b4c091603ebf0cacde0957191632))
* **compiler:** Fix topology for adjacent `on`s [LNG-257] ([#929](https://github.com/fluencelabs/aqua/issues/929)) ([ba15d9e](https://github.com/fluencelabs/aqua/commit/ba15d9e06afc38f79c95e00d48efbf8937cd251d))
* **compiler:** fix typo ([#916](https://github.com/fluencelabs/aqua/issues/916)) ([feb7a16](https://github.com/fluencelabs/aqua/commit/feb7a167a2007d2c1d75dac6554f36d2cf5f86a5))
* **compiler:** Passed function is not handled correctly [LNG-260] ([#940](https://github.com/fluencelabs/aqua/issues/940)) ([c83d69e](https://github.com/fluencelabs/aqua/commit/c83d69e6895d3784f02c743b89f65606c6302f4b))
* **compiler:** Return ability arrow [LNG-258] ([#935](https://github.com/fluencelabs/aqua/issues/935)) ([fab46ee](https://github.com/fluencelabs/aqua/commit/fab46ee130d4e6b3edd33d0fab4c0f2d27ce5ad0))
* **deps:** update dependency @fluencelabs/js-client to v0.2.1 ([#930](https://github.com/fluencelabs/aqua/issues/930)) ([14f3d92](https://github.com/fluencelabs/aqua/commit/14f3d92ef4cd3fed12cbff0164706b2cbbfdcccd))
* **deps:** update dependency @fluencelabs/js-client to v0.3.0 ([#936](https://github.com/fluencelabs/aqua/issues/936)) ([441c012](https://github.com/fluencelabs/aqua/commit/441c012e01e6e0eff1e1e950b86241982658e80a))

## [0.12.3](https://github.com/fluencelabs/aqua/compare/aqua-v0.12.2...aqua-v0.12.3) (2023-09-28)


### Features

* **compiler:** Make `if` propagate errors [fixes LNG-202] ([#779](https://github.com/fluencelabs/aqua/issues/779)) ([ca6cae9](https://github.com/fluencelabs/aqua/commit/ca6cae96ad27f07fe2e6c05ee1caa16153c0c991))
* **compiler:** Replace `%last_error%` with `:error:` [fixes LNG-239] ([#883](https://github.com/fluencelabs/aqua/issues/883)) ([a80033b](https://github.com/fluencelabs/aqua/commit/a80033b81cb7f58f9bfe8665205b864c5438306e))
* Create structs with stream maps [fixes LNG-244] ([#893](https://github.com/fluencelabs/aqua/issues/893)) ([878990a](https://github.com/fluencelabs/aqua/commit/878990a8370718b64bd26663d7813a972e7cdd49))
* **lsp-api:** Add `infoType` ([#915](https://github.com/fluencelabs/aqua/issues/915)) ([1e636cc](https://github.com/fluencelabs/aqua/commit/1e636cc076f5ba4b0487e60fe312ff873d5c19ae))
* **tests:** Add integration test for result error handling ([#914](https://github.com/fluencelabs/aqua/issues/914)) ([b2ca1d3](https://github.com/fluencelabs/aqua/commit/b2ca1d35bf1f6420cea673e092a9f357c6e3a742))


### Bug Fixes

* **compiler:** Handle errors from result handling [fixes LNG-247] ([#913](https://github.com/fluencelabs/aqua/issues/913)) ([f158074](https://github.com/fluencelabs/aqua/commit/f158074c4ecb7d02236786a809c7c387bbb65d3b))

## [0.12.2](https://github.com/fluencelabs/aqua/compare/aqua-v0.12.1...aqua-v0.12.2) (2023-09-25)


### Features

* **aqua-api:** Use scala.js link instead of opt ([#891](https://github.com/fluencelabs/aqua/issues/891)) ([3f916c7](https://github.com/fluencelabs/aqua/commit/3f916c78ab3aa6e50d47929bf28b846bf5537868))
* **build:** Implement custom bundle task ([#894](https://github.com/fluencelabs/aqua/issues/894)) ([67d8151](https://github.com/fluencelabs/aqua/commit/67d8151d9432971b5375794bb15394eeea94132e))
* **compiler:** Add warnings subsystem [fixes LNG117] ([#906](https://github.com/fluencelabs/aqua/issues/906)) ([27a781d](https://github.com/fluencelabs/aqua/commit/27a781dd3f161728c111e5fe834389da69f82d8f))
* **compiler:** Fail on internal error [fixes LNG-229] ([#905](https://github.com/fluencelabs/aqua/issues/905)) ([8741c91](https://github.com/fluencelabs/aqua/commit/8741c910bed18420cf69fd3bf0496b01148e4cf5))
* **compiler:** Services as abilities [fixes LNG-206] ([#873](https://github.com/fluencelabs/aqua/issues/873)) ([6be2a3d](https://github.com/fluencelabs/aqua/commit/6be2a3d5da83c426761819908c0a3c3fd8dc4bf0))
* export types from aqua-api ([#904](https://github.com/fluencelabs/aqua/issues/904)) ([594f465](https://github.com/fluencelabs/aqua/commit/594f46529d7476f8f2345e9deaa43938d7d52a47))
* **language-server:** Pass errors to lsp context [fixes LNG-243] ([#888](https://github.com/fluencelabs/aqua/issues/888)) ([f8b5017](https://github.com/fluencelabs/aqua/commit/f8b5017918a1ec076ccc1593cca70e9e71276b36))
* rethrow errors to capture stacktrace ([#907](https://github.com/fluencelabs/aqua/issues/907)) ([66638af](https://github.com/fluencelabs/aqua/commit/66638afa2d6ad6ca2fba1492c94b2c80fd397e39))


### Bug Fixes

* **compiler:** Do not restrict stream args when ability is present [fixes LNG-233] ([#902](https://github.com/fluencelabs/aqua/issues/902)) ([54ddcc8](https://github.com/fluencelabs/aqua/commit/54ddcc8b6271b1aed4b427dcc74360a0cb212eb4))
* **compiler:** Fix arrows capture in closures [fixes LNG-242] ([#903](https://github.com/fluencelabs/aqua/issues/903)) ([ed9e708](https://github.com/fluencelabs/aqua/commit/ed9e708939d9a689b51ebd0e2ab89a8c92a07b05))
* **deps:** update dependency @fluencelabs/js-client to v0.1.4 ([#887](https://github.com/fluencelabs/aqua/issues/887)) ([d6f879e](https://github.com/fluencelabs/aqua/commit/d6f879ef7ab7e1a8dfadc12b573cf1e51e06d9c9))
* **deps:** update dependency @fluencelabs/js-client to v0.1.6 ([#890](https://github.com/fluencelabs/aqua/issues/890)) ([fb75bc2](https://github.com/fluencelabs/aqua/commit/fb75bc267e9b2cb0c7dbeac389b73ee33b273a11))
* **deps:** update dependency @fluencelabs/js-client to v0.1.7 ([#908](https://github.com/fluencelabs/aqua/issues/908)) ([1c708c8](https://github.com/fluencelabs/aqua/commit/1c708c8bb03a9c7c749b814d412097498c833b36))

## [0.12.1](https://github.com/fluencelabs/aqua/compare/aqua-v0.12.0...aqua-v0.12.1) (2023-09-11)


### Bug Fixes

* **compiler:** Error on not arrow call after `&lt;-` ([#876](https://github.com/fluencelabs/aqua/issues/876)) ([69a808e](https://github.com/fluencelabs/aqua/commit/69a808e24307b5fe312a6dfdc6041c310c33d96d))
* **compiler:** Fix closure stream capture [fixes LNG-58] ([#857](https://github.com/fluencelabs/aqua/issues/857)) ([443e65e](https://github.com/fluencelabs/aqua/commit/443e65e3d8bca4774f5bdb6db5e526c5f2201c89))
* **deps:** update dependency @fluencelabs/aqua-lib to v0.7.3 ([#882](https://github.com/fluencelabs/aqua/issues/882)) ([3419607](https://github.com/fluencelabs/aqua/commit/3419607e8ccd3d280d5d168d6ffb9cb9380d32a8))
* **deps:** update dependency @fluencelabs/js-client to v0.1.1 ([#865](https://github.com/fluencelabs/aqua/issues/865)) ([1f23545](https://github.com/fluencelabs/aqua/commit/1f23545b49db2e3bb387ef9d961cac53bb75d127))
* **deps:** update dependency @fluencelabs/js-client to v0.1.3 ([#875](https://github.com/fluencelabs/aqua/issues/875)) ([df111ad](https://github.com/fluencelabs/aqua/commit/df111adf21c1abe5fbbed7264734927a3f048ffc))

## [0.12.0](https://github.com/fluencelabs/aqua/compare/aqua-v0.11.11...aqua-v0.12.0) (2023-08-25)


### ⚠ BREAKING CHANGES

* **js-client:** Move to new js-client interface ([#855](https://github.com/fluencelabs/aqua/issues/855))

### Features

* **compiler:** Restrict abilities usage [fixes LNG-208] ([#854](https://github.com/fluencelabs/aqua/issues/854)) ([2a0b207](https://github.com/fluencelabs/aqua/commit/2a0b20763396fea0ada5e14c01372dd3923b424b))
* **js-client:** Move to new js-client interface ([#855](https://github.com/fluencelabs/aqua/issues/855)) ([0f9ede0](https://github.com/fluencelabs/aqua/commit/0f9ede09fb849915b20f87fddb95ee2514421a19))


### Bug Fixes

* **compiler:** Fix nested abilities [fixes LNG-220] ([#852](https://github.com/fluencelabs/aqua/issues/852)) ([bf0b51f](https://github.com/fluencelabs/aqua/commit/bf0b51fa5bca3be96cab028eaec48aa5805b1f73))

## [0.11.11](https://github.com/fluencelabs/aqua/compare/aqua-v0.11.10...aqua-v0.11.11) (2023-08-21)


### Features

* **compiler:** Structural typing for data and abilities [fixes LNG-215] ([#843](https://github.com/fluencelabs/aqua/issues/843)) ([019611a](https://github.com/fluencelabs/aqua/commit/019611a89c31618985303d4984ed581eadad11f5))
* parseq implementation (fixes LNG-223) ([#840](https://github.com/fluencelabs/aqua/issues/840)) ([8060695](https://github.com/fluencelabs/aqua/commit/8060695dbb0a2f34febf739eb20db8b8781b3682))


### Bug Fixes

* **compiler:** Generate stream restriction for scoped exprs [fixes LNG-222] ([#841](https://github.com/fluencelabs/aqua/issues/841)) ([eb4cdb0](https://github.com/fluencelabs/aqua/commit/eb4cdb0dd12987e64881bab6ff19f935e905672e))
* **compiler:** Refactor values [fixes LNG-57] ([#821](https://github.com/fluencelabs/aqua/issues/821)) ([f562bd4](https://github.com/fluencelabs/aqua/commit/f562bd40b6df5bbfce5635c10710d91f21e3af88))
* Fix release build command ([#834](https://github.com/fluencelabs/aqua/issues/834)) ([6146f8e](https://github.com/fluencelabs/aqua/commit/6146f8e40a59c9fecd9f40b76e6ec6398b05ca05))

## [0.11.9](https://github.com/fluencelabs/aqua/compare/aqua-v0.11.8...aqua-v0.11.9) (2023-08-09)


### Features

* **compiler:** Add boolean algebra [fixes LNG-211] ([#814](https://github.com/fluencelabs/aqua/issues/814)) ([a5b6102](https://github.com/fluencelabs/aqua/commit/a5b610242260538ff38d62dc21b97a694d0776e3))
* **compiler:** Add equality ops [fixes LNG-217] ([#820](https://github.com/fluencelabs/aqua/issues/820)) ([a5e9354](https://github.com/fluencelabs/aqua/commit/a5e9354aebe9291e9fc0b1d29e74972bfaa254e2))
* **compiler:** Restrict exporting functions that return arrow types or ability types [fixes LNG-209] ([#815](https://github.com/fluencelabs/aqua/issues/815)) ([fabf8d7](https://github.com/fluencelabs/aqua/commit/fabf8d7d61ec8d70bf8e17e581c3c7371c4e6d78))
* wrap aqua api ([#807](https://github.com/fluencelabs/aqua/issues/807)) ([c7fca40](https://github.com/fluencelabs/aqua/commit/c7fca40f670a4b5a51ab4ce188f69f550d4bf6d6))


### Bug Fixes

* **compiler:** Fix `if` with brackets parsing ([#812](https://github.com/fluencelabs/aqua/issues/812)) ([4c3c32b](https://github.com/fluencelabs/aqua/commit/4c3c32b7c400e87f962dc9827892a9224765e2a4))
* **compiler:** Fix math ops for `u64` [fixes LNG-204] ([#811](https://github.com/fluencelabs/aqua/issues/811)) ([50ba194](https://github.com/fluencelabs/aqua/commit/50ba194b8610b60bcaefee401cadacb369246f79))
* **compiler:** Nested abilities [fixes LNG-214] ([#816](https://github.com/fluencelabs/aqua/issues/816)) ([4e3e70f](https://github.com/fluencelabs/aqua/commit/4e3e70f4fc855a16238c4f84bd4f6a1102890904))
* **compiler:** Runtime error on compilation exported functions with top types [fixes LNG-218] ([#822](https://github.com/fluencelabs/aqua/issues/822)) ([ef4b014](https://github.com/fluencelabs/aqua/commit/ef4b0143ac7cd4e1a5997d6a0f1f690ab806a315))

## [0.11.8](https://github.com/fluencelabs/aqua/compare/aqua-v0.11.7...aqua-v0.11.8) (2023-07-20)


### Features

* **compiler:** Abilities ([#731](https://github.com/fluencelabs/aqua/issues/731)) ([63a9f42](https://github.com/fluencelabs/aqua/commit/63a9f42e86d29b741fa31135b4111bc0f38f238f))
* **compiler:** Find and display link cycles ([#787](https://github.com/fluencelabs/aqua/issues/787)) ([667a825](https://github.com/fluencelabs/aqua/commit/667a8255d994b334dfc87bd89a970855748752fe))
* **compiler:** Make `on` propagate errors [fixes LNG-203] ([#788](https://github.com/fluencelabs/aqua/issues/788)) ([b8b0faf](https://github.com/fluencelabs/aqua/commit/b8b0fafda0d27607ffc693e52c0dae81d23ec503))
* **compiler:** Make topology hop with non-FFI snippet [fixes LNG-125] ([#764](https://github.com/fluencelabs/aqua/issues/764)) ([c1fe24b](https://github.com/fluencelabs/aqua/commit/c1fe24b04d8a2f711ed7b316e7ae9a4f12732421))


### Bug Fixes

* **ci:** use unstable nox image ([#780](https://github.com/fluencelabs/aqua/issues/780)) ([22f380a](https://github.com/fluencelabs/aqua/commit/22f380a49162d8d79cccad266b17116d9f9c7795))
* **compiler:** Fix search for one element cycles ([#797](https://github.com/fluencelabs/aqua/issues/797)) ([33ab33d](https://github.com/fluencelabs/aqua/commit/33ab33d4c8f34743202e5acbfb2e976ab3070299))
* **deps:** update dependency @fluencelabs/fluence-network-environment to v1.1.2 ([#786](https://github.com/fluencelabs/aqua/issues/786)) ([ca52e25](https://github.com/fluencelabs/aqua/commit/ca52e2542cc031c748c6f8c8372aff717e9c709f))

## [0.11.7](https://github.com/fluencelabs/aqua/compare/aqua-v0.11.6...aqua-v0.11.7) (2023-06-16)


### Bug Fixes

* **compiler:** Fix incorrect service method renaming [fixes LNG-199] ([#757](https://github.com/fluencelabs/aqua/issues/757)) ([e22fff7](https://github.com/fluencelabs/aqua/commit/e22fff7c8ac3b30baacd4088d96386e73985fd54))

## [0.11.6](https://github.com/fluencelabs/aqua/compare/aqua-v0.11.5...aqua-v0.11.6) (2023-06-15)


### Bug Fixes

* **compiler:** Fix SO in Topology [fixes LNG-149] ([#752](https://github.com/fluencelabs/aqua/issues/752)) ([017eca7](https://github.com/fluencelabs/aqua/commit/017eca70563d82488d6072d0db508253d984e9b2))
* generate JS in Aqua API properly ([#755](https://github.com/fluencelabs/aqua/issues/755)) ([889c507](https://github.com/fluencelabs/aqua/commit/889c50734b56327b39d3d78903d25450bad27fa7))

## [0.11.5](https://github.com/fluencelabs/aqua/compare/aqua-v0.11.4...aqua-v0.11.5) (2023-06-14)


### Bug Fixes

* **compiler:** Fix closure passing [fixes LNG-92] ([#747](https://github.com/fluencelabs/aqua/issues/747)) ([f1abd58](https://github.com/fluencelabs/aqua/commit/f1abd587b761c23b1e27f22bfd8b21ada03a4c5d))
* **parser:** Fix compare ops parsing ([#748](https://github.com/fluencelabs/aqua/issues/748)) ([739854a](https://github.com/fluencelabs/aqua/commit/739854a20b73c7d3cd5eec28f5a7f3ae12691987))

## [0.11.4](https://github.com/fluencelabs/aqua/compare/aqua-v0.11.3...aqua-v0.11.4) (2023-06-13)


### Features

* **tracing:** Introduce function calls tracing [fixes LNG-169] ([#732](https://github.com/fluencelabs/aqua/issues/732)) ([e9c0044](https://github.com/fluencelabs/aqua/commit/e9c004452e48a22dfff2ddd64f1d98797ac6db84))


### Bug Fixes

* **aqua-api:** Compilation to js/ts for file without exports [LNG-196]  ([#744](https://github.com/fluencelabs/aqua/issues/744)) ([8c2240d](https://github.com/fluencelabs/aqua/commit/8c2240d3b16e6f3f968d393aeb1136ffffc9464c))
* **compiler:** Fix closure call compilation [fixes LNG-193] ([#741](https://github.com/fluencelabs/aqua/issues/741)) ([c5534a9](https://github.com/fluencelabs/aqua/commit/c5534a964c21d48bd944e9c62e144cbbbd58625f))

## [0.11.3](https://github.com/fluencelabs/aqua/compare/aqua-v0.11.2...aqua-v0.11.3) (2023-06-09)


### Features

* **devcontainer:** Add js-client to devcontainer ([#736](https://github.com/fluencelabs/aqua/issues/736)) ([62b1642](https://github.com/fluencelabs/aqua/commit/62b16426deabaaadf51ee9cc673da11983535492))


### Bug Fixes

* **aqua-api:** Add default constants to Aqua API [LNG-194] ([#738](https://github.com/fluencelabs/aqua/issues/738)) ([5f5fc49](https://github.com/fluencelabs/aqua/commit/5f5fc494b66f649b440fd332edf8adcd39beb8e8))

## [0.11.2](https://github.com/fluencelabs/aqua/compare/aqua-v0.11.1...aqua-v0.11.2) (2023-06-07)


### Features

* **aqua-api:** return JS and TS sources from API [LNG-164] ([#730](https://github.com/fluencelabs/aqua/issues/730)) ([0b66aa9](https://github.com/fluencelabs/aqua/commit/0b66aa96ece2ce3b1ed44ff1e183e0289b5af2d9))


### Bug Fixes

* **language-server:** Go-to definition for path in `use` expression [LNG-187] ([#733](https://github.com/fluencelabs/aqua/issues/733)) ([21cb393](https://github.com/fluencelabs/aqua/commit/21cb3937acadca8d245b540aac301b130969ac0b))

## [0.11.1](https://github.com/fluencelabs/aqua/compare/aqua-v0.11.0...aqua-v0.11.1) (2023-06-01)


### Bug Fixes

* **inline:** Nullable value in a nested struct [LNG-160] ([#724](https://github.com/fluencelabs/aqua/issues/724)) ([ddb758c](https://github.com/fluencelabs/aqua/commit/ddb758cee0b4a5e87a7648cac9d16b2bbc637a00))

## [0.11.0](https://github.com/fluencelabs/aqua/compare/aqua-v0.10.6...aqua-v0.11.0) (2023-05-29)


### ⚠ BREAKING CHANGES

* add name scopes to if/else/try blocks ([#715](https://github.com/fluencelabs/aqua/issues/715))

### Features

* add name scopes to if/else/try blocks ([#715](https://github.com/fluencelabs/aqua/issues/715)) ([e4205df](https://github.com/fluencelabs/aqua/commit/e4205dfbbf6838f0fd0aae3cb6190b8433c7745a))
* **devcontainer:** Add devcontainer spec [fixes LNG-148] ([#712](https://github.com/fluencelabs/aqua/issues/712)) ([9539c81](https://github.com/fluencelabs/aqua/commit/9539c812364a531dc33db47b618a93e98691439b))
* **parser:** Strict indentation [fixes LNG-135] ([#714](https://github.com/fluencelabs/aqua/issues/714)) ([ae2a433](https://github.com/fluencelabs/aqua/commit/ae2a433185c363386bf54eaebace70b67b8d3e5d))
* use new blueprint ([#708](https://github.com/fluencelabs/aqua/issues/708)) ([d393b71](https://github.com/fluencelabs/aqua/commit/d393b718916b8b81b2440558461f692802a554be))

## [0.10.6](https://github.com/fluencelabs/aqua/compare/aqua-v0.10.5...aqua-v0.10.6) (2023-05-03)


### Bug Fixes

* `ParRes with no children converted to Null` log ([#699](https://github.com/fluencelabs/aqua/issues/699)) ([5f00b1e](https://github.com/fluencelabs/aqua/commit/5f00b1ea8dcb2bc467936053eb3927dfb36850fc))

## [0.10.5](https://github.com/fluencelabs/aqua/compare/aqua-v0.10.4...aqua-v0.10.5) (2023-04-17)


### Features

* Return arrows from functions ([#693](https://github.com/fluencelabs/aqua/issues/693)) ([8fa979c](https://github.com/fluencelabs/aqua/commit/8fa979cd3332d961f013adce332975d3e64317eb))


### Bug Fixes

* **deps:** update dependency @fluencelabs/aqua-api to v0.10.4 ([#671](https://github.com/fluencelabs/aqua/issues/671)) ([c4144ba](https://github.com/fluencelabs/aqua/commit/c4144bad9cc7bf9253323359ca742ba82c0556dd))

## [0.10.4](https://github.com/fluencelabs/aqua/compare/aqua-v0.10.3...aqua-v0.10.4) (2023-04-10)


### Features

* **compiler:** add aqua native compilation ([#681](https://github.com/fluencelabs/aqua/issues/681)) ([5f4808b](https://github.com/fluencelabs/aqua/commit/5f4808b457fbc5f36b64fc4d025700e25f5ccfaa))
* Multiline collections and objects creation ([#684](https://github.com/fluencelabs/aqua/issues/684)) ([f5106e3](https://github.com/fluencelabs/aqua/commit/f5106e3c838585b44503f20002db0013b2325257))
* Remove bang in lambdas in AIR ([#688](https://github.com/fluencelabs/aqua/issues/688)) ([480720f](https://github.com/fluencelabs/aqua/commit/480720f26ed2d5e01e8a73607750fbd2fa69faea))


### Bug Fixes

* Check if expression can be added to a block ([#685](https://github.com/fluencelabs/aqua/issues/685)) ([2394a9b](https://github.com/fluencelabs/aqua/commit/2394a9b875f5de37bf3edf267c6b8f26a20f959e))
* **deps:** update dependency @fluencelabs/fluence-network-environment to v1.0.14 ([#669](https://github.com/fluencelabs/aqua/issues/669)) ([f2713a4](https://github.com/fluencelabs/aqua/commit/f2713a4ab1a10725fce0ebb15321797326c4679a))

## [0.10.3](https://github.com/fluencelabs/aqua/compare/aqua-v0.10.2...aqua-v0.10.3) (2023-02-28)


### Bug Fixes

* Fix type for functor [fixes LNG-119] ([#677](https://github.com/fluencelabs/aqua/issues/677)) ([bb24a63](https://github.com/fluencelabs/aqua/commit/bb24a63abb2849ea1df756f6a2a2fc5f6b408309))

## [0.10.2](https://github.com/fluencelabs/aqua/compare/aqua-v0.10.1...aqua-v0.10.2) (2023-02-25)


### Features

* **docs:** README update ([#667](https://github.com/fluencelabs/aqua/issues/667)) ([75c7135](https://github.com/fluencelabs/aqua/commit/75c7135e12757f6fef5c0fd3ce62bfc45607298b))


### Bug Fixes

* name uniqueness in stream gate ([#676](https://github.com/fluencelabs/aqua/issues/676)) ([00ee8b5](https://github.com/fluencelabs/aqua/commit/00ee8b531265a8e476b683d1a4cadd7e51ec3de3))

## [0.10.1](https://github.com/fluencelabs/aqua/compare/aqua-v0.10.0...aqua-v0.10.1) (2023-02-16)


### Bug Fixes

* long strings JSON parsing ([#672](https://github.com/fluencelabs/aqua/issues/672)) ([ee0b967](https://github.com/fluencelabs/aqua/commit/ee0b9673ef5281bfad74ef83301353ca25c0017b))

## [0.10.0](https://github.com/fluencelabs/aqua/compare/aqua-v0.9.5...aqua-v0.10.0) (2023-02-16)


### ⚠ BREAKING CHANGES

* DXJ-283 support new JS client ([#668](https://github.com/fluencelabs/aqua/issues/668))

### Features

* DXJ-283 support new JS client ([#668](https://github.com/fluencelabs/aqua/issues/668)) ([94cbb85](https://github.com/fluencelabs/aqua/commit/94cbb85b407d67d5c5da52db91f50d41874ab83b))

## [0.9.5](https://github.com/fluencelabs/aqua/compare/aqua-v0.9.4...aqua-v0.9.5) (2023-02-14)


### Bug Fixes

* **deps:** update dependency @fluencelabs/aqua-ipfs to v0.5.9 ([#661](https://github.com/fluencelabs/aqua/issues/661)) ([b49e897](https://github.com/fluencelabs/aqua/commit/b49e897ba444b0991a9c889b9b1e1816eb3f1780))

## [0.9.4](https://github.com/fluencelabs/aqua/compare/aqua-v0.9.3...aqua-v0.9.4) (2023-02-01)


### Bug Fixes

* Canonicalize variable in object creation or copy if variable is a stream ([#649](https://github.com/fluencelabs/aqua/issues/649)) ([fedd743](https://github.com/fluencelabs/aqua/commit/fedd743721c33ccee51b2b6b8efff2b032586329))


### Performance Improvements

* Unfold variables in parallel where it is possible (fixes LNG-109 ) ([#656](https://github.com/fluencelabs/aqua/issues/656)) ([439f2cd](https://github.com/fluencelabs/aqua/commit/439f2cde03b5bca99b072bf7cad389168b8ad0fa))

## [0.9.3](https://github.com/fluencelabs/aqua/compare/aqua-v0.9.2...aqua-v0.9.3) (2023-01-20)


### Features

* copy structures implementation [fixes LNG-102] ([#646](https://github.com/fluencelabs/aqua/issues/646)) ([50f0723](https://github.com/fluencelabs/aqua/commit/50f0723a321c76490587ea5350b4055ae2f470ec))


### Bug Fixes

* `nil` in return [DXJ-226] ([#629](https://github.com/fluencelabs/aqua/issues/629)) ([7ab980a](https://github.com/fluencelabs/aqua/commit/7ab980a5f00ba6f529a13faced3a25c04df19717))
* **ci:** Set correct aqua version when creating release-please PRs [fixes FLU-215 and FLU-218] [#642](https://github.com/fluencelabs/aqua/issues/642) ([d7956c1](https://github.com/fluencelabs/aqua/commit/d7956c1c8d3b3b0367e7db0831ef665df9bad790))
* **error-messages:** fix incorrect number of arguments message [fixes LNG-108] ([#645](https://github.com/fluencelabs/aqua/issues/645)) ([d0a9db5](https://github.com/fluencelabs/aqua/commit/d0a9db51640283ab065f7a7a5b5078f1f8ac7c29))

## [0.9.2](https://github.com/fluencelabs/aqua/compare/aqua-v0.9.1...aqua-v0.9.2) (2023-01-17)


### Bug Fixes

* **deps:** update dependency @fluencelabs/fluence to v0.28.0 [#627](https://github.com/fluencelabs/aqua/issues/627) ([fa1dc89](https://github.com/fluencelabs/aqua/commit/fa1dc89313d63830b7858cb10004a07dba6694c9))
