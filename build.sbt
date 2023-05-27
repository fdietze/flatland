Global / onChangedBuildSource := ReloadOnSourceChanges

val sharedSettings = Seq(
  crossScalaVersions                              := Seq("2.12.17", "2.13.10", "3.1.3"),
  scalaVersion                                    := crossScalaVersions.value.last,
  resolvers ++=
    ("jitpack" at "https://jitpack.io") ::
      Nil,
  libraryDependencies += "org.scala-lang.modules" %% "scala-collection-compat" % "2.9.0",
  scalacOptions --= Seq("-Xfatal-warnings"), // overwrite sbt-tpolecat setting
)

lazy val flatland = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(sharedSettings)
  .settings(
    organization                           := "com.github.fdietze",
    name                                   := "flatland",
    version                                := "master-SNAPSHOT",
    libraryDependencies ++= (
      "org.scalatest"                     %%% "scalatest"       % "3.2.16"   % Test ::
        "org.scalatestplus"               %%% "scalacheck-1-16" % "3.2.14.0" % Test ::
        "org.scalacheck"                  %%% "scalacheck"      % "1.17.0"   % Test ::
        Nil
    ),
    Test / scalaJSStage                    := FullOptStage,
    console / initialCommands              := """
      import flatland._
      """,
    Compile / packageDoc / publishArtifact := false, // disable publishing docs, because of a crash
  )

lazy val bench = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(sharedSettings)
  .dependsOn(flatland)
  .settings(
    publish / skip                := true,
    libraryDependencies ++=
      "com.github.fdietze.bench" %%% "bench" % "ed4b6ed" ::
        Nil,
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, major)) if major == 11 => (
          "-Xdisable-assertions" ::
            /* "-optimise" :: */
            /* "-Yclosure-elim" :: */
            /* "-Yinline" :: */
            Nil
        )
        case Some((2, major)) if major >= 12 => (
          "-Xdisable-assertions" ::
            "-opt:l:method" ::
            "-opt:l:inline" ::
            "-opt-inline-from:**" ::
            Nil
        )
        case _                               => Seq.empty
      }
    },
  )
  .jsSettings(
    Compile / scalaJSStage          := FullOptStage,
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
  )
