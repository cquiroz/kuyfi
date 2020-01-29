import sbt.Keys._

Global / onChangedBuildSource := ReloadOnSourceChanges

val commonSettings: Seq[Setting[_]] = Seq(
  organization := "io.github.cquiroz",
  scalaVersion := "2.13.1",
  crossScalaVersions := Seq("2.12.10", "2.13.1")
)

lazy val kuyfi: Project = project.in(file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "kuyfi",
    Test / run / fork := true,
    libraryDependencies ++= Seq(
      "org.tpolecat"         %% "atto-core"            % "0.7.2",
      "org.typelevel"        %% "cats-core"            % "2.1.0",
      "org.typelevel"        %% "cats-effect"          % "2.0.0",
      "org.typelevel"        %% "mouse"                % "0.24",
      "com.chuusai"          %% "shapeless"            % "2.3.3",
      "com.github.pathikrit" %% "better-files"         % "3.8.0",
      "com.eed3si9n"         %% "treehugger"           % "0.4.4",
      "org.scalatest"        %% "scalatest"            % "3.1.0" % "test"
    ),
    scalacOptions ~= (_.filterNot(Set(
      // Some overloaded methods don't use all params
      "-Ywarn-unused:params"
    )))
  )

  lazy val docs = project.in(file("docs")).dependsOn(kuyfi)
    .settings(commonSettings)
    .settings(name := "docs")
    .enablePlugins(MicrositesPlugin)
    .settings(
      micrositeName             := "kuyfi",
      micrositeAuthor           := "Carlos Quiroz",
      micrositeGithubOwner      := "cquiroz",
      micrositeGithubRepo       := "kuyfi",
      micrositeBaseUrl          := "/kuyfi",
      //micrositeDocumentationUrl := "/scala-java-time/docs/",
      micrositeHighlightTheme   := "color-brewer"
    )
