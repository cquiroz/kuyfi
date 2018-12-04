import sbt.Keys._

val commonSettings: Seq[Setting[_]] = Seq(
  organization := "io.github.cquiroz",
  scalaVersion := "2.12.8",
  scalacOptions := Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8", // Specify character encoding used by source files.
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds", // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings", // Fail the compilation if there are any warnings.
    "-Xfuture", // Turn on future language features.
    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
    "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
    "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
    "-Xlint:option-implicit", // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match", // Pattern match may not be typesafe.
    "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification", // Enable partial unification in type constructor inference
    "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
    "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",  // Warn if a local definition is unused.
    // "-Ywarn-unused:params",              // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates", // Warn if a private member is unused.
    "-Ywarn-value-discard", // Warn when non-Unit expression results are unused.
    "-Yrangepos"
  ),
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
    Test / run / fork := true,
    libraryDependencies ++= Seq(
      "org.tpolecat"         %% "atto-core"            % "0.6.4",
      "org.typelevel"        %% "cats-core"            % "1.5.0",
      "org.typelevel"        %% "cats-effect"          % "1.1.0",
      "org.typelevel"        %% "mouse"                % "0.19",
      "com.chuusai"          %% "shapeless"            % "2.3.3",
      "com.github.pathikrit" %% "better-files"         % "3.6.0",
      "com.eed3si9n"         %% "treehugger"           % "0.4.3",
      "org.scalatest"        %% "scalatest"            % "3.0.5" % "test"
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
