# Changelog

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
