// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.CrossPlugin.autoImport.{ crossProject, CrossType }

val crossScalaVersionList = Seq("2.11.12", "2.12.9", "2.13.0")
val scalaMajorVersion = SettingKey[Int]("scalaMajorVersion")
val sharedSettings = Seq(
  crossScalaVersions := crossScalaVersionList,
  scalaVersion := crossScalaVersionList.last,
  scalacOptions ++=
    "-encoding" :: "UTF-8" ::
    "-unchecked" ::
    "-deprecation" ::
    "-explaintypes" ::
    "-feature" ::
    "-language:_" ::
    "-Xcheckinit" ::
    /* "-Xfuture" :: */
    /* "-Xlint:-unused" :: */
    /* "-Ypartial-unification" :: */
    /* "-Yno-adapted-args" :: */
    /* "-Ywarn-infer-any" :: */
    "-Ywarn-value-discard" ::
    /* "-Ywarn-nullary-override" :: */
    /* "-Ywarn-nullary-unit" :: */
    Nil,

  resolvers ++=
    ("jitpack" at "https://jitpack.io") ::
    Nil,

  libraryDependencies += "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.2",

  /* scalafixDependencies in ThisBuild += "org.scala-lang.modules" %% "scala-collection-migrations" % "2.0.0", */
  /* scalacOptions ++= List("-Yrangepos", "-P:semanticdb:synthetics:on"), */
  /* addCompilerPlugin(scalafixSemanticdb), */
  scalaMajorVersion := {
    val v = scalaVersion.value
    CrossVersion.partialVersion(v).map(_._2.toInt).getOrElse {
      throw new RuntimeException(s"could not get Scala major version from $v")
    }
  }
)

lazy val flatland = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .settings(sharedSettings)
  .settings(
    organization := "com.github.fdietze",
    name := "flatland",
    version := "master-SNAPSHOT",
    libraryDependencies ++= (
      "org.scalatest" %%% "scalatest" % "3.0.8" % Test ::
      "org.scalacheck" %%% "scalacheck" % "1.14.2" % Test ::
      Nil
    ),

    scalaJSStage in Test := FullOptStage,

    initialCommands in console := """
      import flatland._
      """,
  )
  .jsSettings(
    scalacOptions += {
      val local = baseDirectory.value.toURI
      val remote = s"https://raw.githubusercontent.com/fdietze/flatland/${git.gitHeadCommit.value.get}/"
      s"-P:scalajs:mapSourceURI:$local->$remote"
    }
  )

lazy val bench = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .settings(sharedSettings)
  .dependsOn(flatland)
  .settings(
    version := "0.1.0",
    libraryDependencies ++=
      "com.github.fdietze.bench" %%% "bench" % "87f4b74" ::
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
        case _ => Seq.empty
      }
    },
  )
  .jsSettings(
    scalaJSStage in Compile := FullOptStage,
    scalaJSUseMainModuleInitializer := true,
    scalaJSModuleKind := ModuleKind.CommonJSModule,
  )

Global / onChangedBuildSource := ReloadOnSourceChanges
