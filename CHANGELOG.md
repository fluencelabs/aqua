# Changelog

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
