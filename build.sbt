import sbt.Keys._

val commonSettings: Seq[Setting[_]] = Seq(
  version := s"0.1.0",
  organization := "com.github.cquiroz",
  scalaVersion := "2.10.6",
  crossScalaVersions := Seq("2.10.6", "2.11.8"),
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
    <scm>
      <url>git@github.com:cquiroz/kuyfi.git</url>
      <connection>scm:git:git@github.com:cquiroz/kuyfi.git</connection>
    </scm>
    <developers>
      <developer>
        <id>cquiroz</id>
        <name>Carlos Quiroz</name>
        <url>https://github.com/cquiroz/</url>
      </developer>
    </developers>
  ,
  pomIncludeRepository := { _ => false }
)

lazy val kuyfi: Project = project.in(file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "kuyfi",
    publish := {},
    publishLocal := {},
    libraryDependencies ++= Seq(
      "org.tpolecat"         %% "atto-core"            % "0.5.1",
      "org.tpolecat"         %% "atto-compat-scalaz72" % "0.5.1",
      "org.scalaz"           %% "scalaz-core"          % "7.2.7",
      "org.scalaz"           %% "scalaz-effect"        % "7.2.7",
      "com.chuusai"          %% "shapeless"            % "2.3.2",
      "com.github.pathikrit" %% "better-files"         % "2.14.0",
      "com.eed3si9n"         %% "treehugger"           % "0.4.1",
      "org.scalatest"        %% "scalatest"            % "3.0.0" % "test"
    )
  )
