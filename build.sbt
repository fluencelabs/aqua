import BundleJS.*

val aquaVersion = "0.12.4"

val scalaV = "3.3.1"
val catsV = "2.10.0"
val catsParseV = "0.3.10"
val monocleV = "3.1.0"
val scalaTestV = "3.2.17"
val sourcecodeV = "0.3.0"
val fs2V = "3.9.2"
val catsEffectV = "3.6-1f95fd7"
val declineV = "2.3.0"
val circeVersion = "0.14.2"
val scribeV = "3.7.1"

name := "aqua-hll"

val commons = Seq(
  version := {
    val aquaSnapshot = sys.env.getOrElse("SNAPSHOT", "")
    if (aquaSnapshot.isEmpty()) aquaVersion else aquaVersion + "-" + aquaSnapshot,
  },
  scalaVersion := scalaV,
  libraryDependencies ++= Seq(
    "com.outr"      %%% "scribe"    % scribeV,
    "org.scalatest" %%% "scalatest" % scalaTestV % Test
  ),
  scalacOptions ++= {
    Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-language:implicitConversions",
      "-unchecked",
      "-Ykind-projector"
      //      "-Xfatal-warnings"
    )
  }
)

commons

lazy val `aqua-run` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("aqua-run"))
  .settings(commons)
  .dependsOn(compiler, `backend-air`, `backend-ts`, io, definitions, logging, constants)

lazy val io = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % catsEffectV,
      "co.fs2"        %%% "fs2-io"      % fs2V
    )
  )
  .dependsOn(compiler, parser)

lazy val ioJS = io.js.dependsOn(`js-imports`)

lazy val `language-server-api` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("language-server/language-server-api"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % catsEffectV,
      "co.fs2"        %%% "fs2-io"      % fs2V
    )
  )
  .dependsOn(compiler, io)

lazy val `language-server-apiJS` = `language-server-api`.js
  .settings(
    scalaJSLinkerConfig             ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    scalaJSUseMainModuleInitializer := true
  )
  .settings(addBundleJS("../../language-server-npm/aqua-lsp-api.js"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(`js-exports`, `js-imports`)

lazy val `js-exports` = project
  .in(file("js/js-exports"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commons)
  .dependsOn(`backend`.js, definitions.js)

lazy val `js-imports` = project
  .in(file("js/js-imports"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commons)
  .dependsOn(`js-exports`, transform.js)

lazy val `aqua-api` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("api/api"))
  .settings(commons)
  .dependsOn(`aqua-run`, `backend-api`)

lazy val `aqua-apiJS` = `aqua-api`.js
  .settings(
    scalaJSLinkerConfig             ~= (_.withModuleKind(ModuleKind.ESModule)),
    scalaJSUseMainModuleInitializer := true,
    Test / test                     := {}
  )
  .settings(addBundleJS("../../api-npm/aqua-api.js"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(`js-exports`)

lazy val types = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV
    )
  )
  .dependsOn(errors)

lazy val parser = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-parse" % catsParseV,
      "org.typelevel" %%% "cats-free"  % catsV
    )
  )
  .dependsOn(types, helpers)

lazy val linker = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons)
  .dependsOn(parser)

lazy val tree = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/tree"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-free" % catsV
    )
  )
  .dependsOn(helpers)

lazy val raw = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/raw"))
  .settings(commons)
  .dependsOn(types, tree)

lazy val model = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons)
  .dependsOn(types, tree, raw, helpers, errors)

lazy val res = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/res"))
  .settings(commons)
  .dependsOn(model)

lazy val inline = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/inline"))
  .settings(commons)
  .dependsOn(raw, model)

lazy val transform = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/transform"))
  .settings(commons)
  .dependsOn(model, res, inline, res % "test->test")

lazy val semantics = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "dev.optics" %%% "monocle-core"  % monocleV,
      "dev.optics" %%% "monocle-macro" % monocleV
    )
  )
  .dependsOn(raw, parser, errors)

lazy val compiler = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("compiler"))
  .settings(commons)
  .dependsOn(semantics, linker, backend, transform % "test->test", res % "test->test")

lazy val backend = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend"))
  .settings(commons)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys    := Seq[BuildInfoKey](version),
    buildInfoPackage := "aqua.backend"
  )
  .dependsOn(res, definitions)

lazy val definitions = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/definitions"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion)
  )
  .dependsOn(res, types)

lazy val logging = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("utils/logging"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV
    )
  )

lazy val constants = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("utils/constants"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV
    )
  )
  .dependsOn(parser, raw)

lazy val helpers = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("utils/helpers"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV,
      "org.typelevel" %%% "cats-free" % catsV
    )
  )

lazy val errors = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("utils/errors"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "sourcecode" % sourcecodeV
    )
  )

lazy val `backend-air` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/air"))
  .settings(commons)
  .dependsOn(backend, transform)

lazy val `backend-api` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/api"))
  .settings(commons)
  .dependsOn(backend, transform, `backend-air`)

lazy val `backend-ts` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/ts"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion)
  )
  .dependsOn(`backend-air`, definitions)
