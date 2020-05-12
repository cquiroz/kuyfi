import sbt.Keys._

Global / onChangedBuildSource := ReloadOnSourceChanges

// sbt-ci-release
inThisBuild(
  List(
    homepage := Some(url("https://github.com/cquiroz/kuyfi")),
    licenses := Seq("BSD 3-Clause License" -> url("https://opensource.org/licenses/BSD-3-Clause")),
    developers := List(
      Developer("cquiroz",
                "Carlos Quiroz",
                "carlos.m.quiroz@gmail.com",
                url("https://github.com/cquiroz"))
    ),
    scmInfo := Some(
      ScmInfo(url("https://github.com/cquiroz/kuyfi"), "scm:git:git@github.com:cquiroz/kuyfi.git")
    )
  )
)

val commonSettings: Seq[Setting[_]] = Seq(
  organization := "io.github.cquiroz",
  scalaVersion := "2.13.2",
  crossScalaVersions := Seq("2.12.10", "2.13.2"),
  description := "TZDB parser"
)

lazy val kuyfi: Project = project
  .in(file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "kuyfi",
    Test / run / fork := true,
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "atto-core" % "0.8.0",
      "org.typelevel" %% "cats-core" % "2.1.1",
      "org.typelevel" %% "cats-effect" % "2.1.3",
      "org.typelevel" %% "mouse" % "0.24",
      "com.chuusai" %% "shapeless" % "2.3.3",
      "com.eed3si9n" %% "treehugger" % "0.4.4",
      "org.scalatest" %% "scalatest" % "3.1.2" % "test",
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.6"
    ),
    scalacOptions ~= (_.filterNot(
      Set(
        // Some overloaded methods don't use all params
        "-Ywarn-unused:params"
      )
    ))
  )

lazy val docs = project
  .in(file("docs"))
  .dependsOn(kuyfi)
  .settings(commonSettings)
  .settings(name := "docs")
  .enablePlugins(MicrositesPlugin)
  .settings(
    micrositeName := "kuyfi",
    micrositeAuthor := "Carlos Quiroz",
    micrositeGithubOwner := "cquiroz",
    micrositeGithubRepo := "kuyfi",
    micrositeBaseUrl := "/kuyfi",
    //micrositeDocumentationUrl := "/scala-java-time/docs/",
    micrositeHighlightTheme := "color-brewer"
  )
