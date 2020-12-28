import ReleaseTransformations._

lazy val commonSettings = Seq(

  organization := "org.clulab",

  // Default to 2.12, but still cross-build for 2.11.
  // 2.12.12 results in an exception when trying to access
  // a resource through getResource().  There might be a
  // change related to the leading / or something similar.
  scalaVersion := "2.12.8",

  crossScalaVersions := Seq("2.11.12", "2.12.8"),

  scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation"),

  testOptions in Test += Tests.Argument("-oD"),

  parallelExecution in Global := false,

  // publish to a maven repo
  publishMavenStyle := true,

  // the standard maven repository
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },

  // let’s remove any repositories for optional dependencies in our artifact
  pomIncludeRepository := { _ => false },

  // mandatory stuff to add to the pom for publishing
  pomExtra :=
    <url>https://github.com/clulab/reach</url>
    <licenses>
      <license>
        <name>Apache License, Version 2.0</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>https://github.com/clulab/reach</url>
      <connection>https://github.com/clulab/reach</connection>
    </scm>
    <developers>
      <developer>
        <id>mihai.surdeanu</id>
        <name>Mihai Surdeanu</name>
        <email>mihai@surdeanu.info</email>
      </developer>
    </developers>

  //
  // end publishing settings
  //

)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(main, causalAssembly, export)
  .dependsOn(main % "test->test;compile", causalAssembly, export) // so that we can import from the console
  .settings(
    name := "reach-exe",
    aggregate in test := false,
    aggregate in assembly := false,
    test in assembly := {},
    assemblyMergeStrategy in assembly := {
      // The following line of code deals with these two conflicting files by thowing both away.
      // [error] (root/*:assembly) deduplicate: different file contents found in the following:
      // [error] ~/.ivy2/cache/com.sun.xml.bind/jaxb-impl/jars/jaxb-impl-2.4.0-b180830.0438.jar:module-info.class
      // [error] ~/.ivy2/cache/javax.xml.bind/jaxb-api/jars/jaxb-api-2.4.0-b180830.0359.jar:module-info.class
      case PathList("module-info.class") => MergeStrategy.discard
      case other => (assemblyMergeStrategy in assembly).value(other)
    },
    mainClass in assembly := {
      val sysMain = System.getProperty("mainClass")
      Option(if (sysMain != null) sysMain else "org.clulab.reach.RunReachCLI")
    },
    assemblyJarName in assembly := s"reach-${version.value}-FAT.jar"
  )

// this stores BioNLPProcessor and its models
lazy val processors = project
  .settings(commonSettings:_*)

lazy val main = project
  .settings(commonSettings:_*)
  .dependsOn(processors % "test->test;compile->compile")

lazy val causalAssembly = project.in(file("assembly"))
  .settings(commonSettings:_*)
  .dependsOn(main % "test->test;compile->compile")

lazy val export = project
  .settings(commonSettings:_*)
  .dependsOn(main % "test->test;compile->compile", causalAssembly % "test;compile") // need access to assembly/src/resources

//
// publishing settings
//

// these are the steps to be performed during release
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("+publishSigned"),
  setNextVersion,
  commitNextVersion,
  releaseStepCommandAndRemaining("sonatypeReleaseAll"),
  pushChanges
)
