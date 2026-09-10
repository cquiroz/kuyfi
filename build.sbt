Global / onChangedBuildSource := ReloadOnSourceChanges

// sbt-ci-release
homepage      := Some(uri("https://github.com/cquiroz/kuyfi"))
licenses      := Seq("BSD 3-Clause License" -> uri("https://opensource.org/licenses/BSD-3-Clause"))
developers    := List(
  Developer("cquiroz",
            "Carlos Quiroz",
            "carlos.m.quiroz@gmail.com",
            uri("https://github.com/cquiroz")
  )
)
scmInfo       := Some(
  ScmInfo(uri("https://github.com/cquiroz/kuyfi"), "scm:git:git@github.com:cquiroz/kuyfi.git")
)
versionScheme := Some("early-semver")

val commonSettings: Seq[Setting[?]] = Seq(
  organization := "io.github.cquiroz",
  description  := "TZDB parser"
)

lazy val kuyfi = (projectMatrix in file("."))
  .settings(commonSettings*)
  .settings(
    name              := "kuyfi",
    Test / run / fork := true,
    libraryDependencies ++= Seq(
      "org.typelevel"          %% "cats-parse"              % "1.1.0",
      "org.typelevel"          %% "cats-core"               % "2.13.0",
      "com.eed3si9n"           %% "treehugger"              % "0.5.0",
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.14.0",
      "org.scalameta"          %% "munit"                   % "1.3.6" % Test
    ),
    testFrameworks += new TestFramework("munit.Framework"),
    scalacOptions ~= (_.filterNot(
      Set(
        // Some overloaded methods don't use all params
        "-Ywarn-unused:params"
      )
    ))
  )
  .jvmPlatform(scalaVersions = Seq("3.3.8", "2.13.18", "2.12.21"))

// projectMatrix lives at file("."), so the aggregating root would otherwise pick up
// src/ as its own sources and compile them without the library dependencies.
lazy val root = (project in file("."))
  .aggregate(kuyfi.componentProjects.map(p => p: ProjectReference)*)
  .settings(
    name                                   := "kuyfi-root",
    publish / skip                         := true,
    Compile / unmanagedSourceDirectories   := Nil,
    Test / unmanagedSourceDirectories      := Nil,
    Compile / unmanagedResourceDirectories := Nil,
    Test / unmanagedResourceDirectories    := Nil
  )
