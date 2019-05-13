// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.CrossPlugin.autoImport.{ crossProject, CrossType }

val crossScalaVersionList = Seq("2.10.7", "2.11.12", "2.12.8")
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
    "-Xfuture" ::
    "-Xlint:-unused" ::
    "-Ypartial-unification" ::
    "-Yno-adapted-args" ::
    "-Ywarn-infer-any" ::
    "-Ywarn-value-discard" ::
    "-Ywarn-nullary-override" ::
    "-Ywarn-nullary-unit" ::
    Nil,

  resolvers ++=
    ("jitpack" at "https://jitpack.io") ::
    Nil,
)

lazy val flatland = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .settings(sharedSettings)
  .settings(
    organization := "com.github.fdietze",
    name := "flatland",
    version := "master-SNAPSHOT",
    libraryDependencies ++= (
      "org.scalatest" %%% "scalatest" % "3.0.7" % Test ::
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test ::
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
    crossScalaVersions := crossScalaVersionList,
    scalaVersion := crossScalaVersionList.last,
    libraryDependencies ++=
      "com.github.fdietze.bench" %%% "bench" % "e66a721" ::
      Nil,
    scalacOptions ++=
      "-Xdisable-assertions" ::
      "-opt:l:method" ::
      "-opt:l:inline" ::
      "-opt-inline-from:**" ::
      Nil,
  )

  .jsSettings(
    scalaJSStage in Compile := FullOptStage,
    scalaJSUseMainModuleInitializer := true,
    scalaJSModuleKind := ModuleKind.CommonJSModule,
  )
