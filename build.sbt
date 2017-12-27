import sbt.Keys._

val commonSettings: Seq[Setting[_]] = Seq(
  organization := "io.github.cquiroz",
  scalaVersion := "2.12.4",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-Xfatal-warnings",
    "-encoding", "UTF-8"),
  exportJars := true,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra :=
    <url>https://github.com/cquiroz/kuyfi</url>
    <licenses>
      <license>
        <name>BSD-style</name>
        <url>http://www.opensource.org/licenses/bsd-license.php</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <developers>
      <developer>
        <id>cquiroz</id>
        <name>Carlos Quiroz</name>
        <url>https://github.com/cquiroz/</url>
      </developer>
    </developers>
  ,
  pomIncludeRepository := { _ => false },
  // Settings to use git to define the version of the project
  git.useGitDescribe := true,
  git.formattedShaVersion := git.gitHeadCommit.value map { sha => s"v$sha" },
  git.uncommittedSignifier in ThisBuild := Some("UNCOMMITTED"),
  useGpg := true
)

lazy val kuyfi: Project = project.in(file("."))
  .enablePlugins(GitVersioning)
  .enablePlugins(GitBranchPrompt)
  .settings(commonSettings: _*)
  .settings(
    name := "kuyfi",
    libraryDependencies ++= Seq(
      "org.tpolecat"         %% "atto-core"            % "0.6.1",
      "org.typelevel"        %% "cats-core"            % "1.0.0",
      "org.typelevel"        %% "cats-effect"          % "0.6",
      "org.typelevel"        %% "mouse"                % "0.15",
      "com.chuusai"          %% "shapeless"            % "2.3.2",
      "com.github.pathikrit" %% "better-files"         % "2.17.1",
      "com.eed3si9n"         %% "treehugger"           % "0.4.3",
      "org.scalatest"        %% "scalatest"            % "3.0.4" % "test"
    )
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
