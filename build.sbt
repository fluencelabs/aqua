val dottyVersion = "3.3.0"

scalaVersion := dottyVersion

val aquaVersion = "0.11.4"

val catsV = "2.8.0"
val catsParseV = "0.3.8"
val monocleV = "3.1.0"
val scalaTestV = "3.2.16"
val fs2V = "3.7.0"
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
  scalaVersion := dottyVersion,
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

lazy val cli = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("cli/cli"))
  .enablePlugins(GraalVMNativeImagePlugin)
  .settings(commons: _*)
  .settings(
    Compile / mainClass := Some("aqua.AquaCli"),
    graalVMNativeImageOptions ++= Seq(
      "--no-fallback",
      "--diagnostics-mode",
      "--initialize-at-build-time",
      "--initialize-at-run-time=scala.util.Random$",
      "-H:-DeleteLocalSymbols",
      "-H:+PreserveFramePointer",
      "-H:+ReportExceptionStackTraces",
      "-H:+DashboardHeap",
      "-H:+DashboardCode",
      "-H:+DashboardPointsTo",
      "-H:+DashboardAll"
    ) ++ sys.env
      .get("COMPILE_STATIC")
      .filter(_.trim.toLowerCase() == "true")
      .map(_ => Seq("--static"))
      .getOrElse(Seq.empty),
    libraryDependencies ++= Seq(
      "com.monovore" %%% "decline"        % declineV,
      "com.monovore" %%% "decline-effect" % declineV
    )
  )
  .dependsOn(compiler, `backend-air`, `backend-ts`, io, definitions, logging, constants, `aqua-run`)

lazy val cliJS = cli.js
  .settings(
    scalaJSLinkerConfig             ~= (_.withModuleKind(ModuleKind.ESModule)),
    scalaJSUseMainModuleInitializer := true
  )
  .dependsOn(`js-exports`, `js-imports`)

lazy val cliJVM = cli.jvm
  .settings(
    Compile / run / mainClass  := Some("aqua.AquaCli"),
    assembly / mainClass       := Some("aqua.AquaCli"),
    assembly / assemblyJarName := "aqua-" + version.value + ".jar",
    libraryDependencies ++= Seq(
    )
  )

lazy val `aqua-run` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("aqua-run"))
  .settings(commons: _*)
  .dependsOn(compiler, `backend-air`, `backend-ts`, io, definitions, logging, constants)

lazy val io = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons: _*)
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
  .settings(commons: _*)
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
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(`js-exports`, `js-imports`)

lazy val `js-exports` = project
  .in(file("js/js-exports"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commons: _*)
  .dependsOn(`backend`.js, definitions.js)

lazy val `js-imports` = project
  .in(file("js/js-imports"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commons: _*)
  .dependsOn(`js-exports`, transform.js)

lazy val `aqua-api` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("api/aqua-api"))
  .settings(commons: _*)
  .dependsOn(`aqua-run`, `backend-api`)

lazy val `aqua-apiJS` = `aqua-api`.js
  .settings(
    scalaJSLinkerConfig             ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    scalaJSUseMainModuleInitializer := true,
    Test / test                     := {}
  )
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

lazy val parser = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-parse" % catsParseV,
      "org.typelevel" %%% "cats-free"  % catsV
    )
  )
  .dependsOn(types)

lazy val linker = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons: _*)
  .dependsOn(parser)

lazy val tree = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/tree"))
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-free" % catsV
    )
  )

lazy val raw = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/raw"))
  .settings(commons: _*)
  .dependsOn(types, tree)

lazy val model = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons: _*)
  .dependsOn(types, tree, raw)

lazy val res = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/res"))
  .settings(commons: _*)
  .dependsOn(model)

lazy val inline = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/inline"))
  .settings(commons: _*)
  .dependsOn(raw, model)

lazy val transform = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/transform"))
  .settings(commons: _*)
  .dependsOn(model, res, inline)

lazy val semantics = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "dev.optics" %%% "monocle-core"  % monocleV,
      "dev.optics" %%% "monocle-macro" % monocleV
    )
  )
  .dependsOn(raw, parser)

lazy val compiler = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("compiler"))
  .settings(commons: _*)
  .dependsOn(semantics, linker, backend, transform % Test)

lazy val backend = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend"))
  .settings(commons: _*)
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
  .settings(commons: _*)
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
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV
    )
  )

lazy val constants = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("utils/constants"))
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV
    )
  )
  .dependsOn(parser, raw)

lazy val `backend-air` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/air"))
  .settings(commons: _*)
  .dependsOn(backend, transform)

lazy val `backend-api` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/api"))
  .settings(commons: _*)
  .dependsOn(backend, transform, `backend-air`)

lazy val `backend-ts` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/ts"))
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion)
  )
  .dependsOn(`backend-air`, definitions)
