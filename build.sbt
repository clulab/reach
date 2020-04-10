import ReleaseTransformations._

lazy val commonSettings = Seq(

  organization := "org.clulab",

  // FIXME: cross-build for 2.12!
  scalaVersion := "2.11.11",

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
  .aggregate(main, export)
  .dependsOn(main % "test->test;compile", export) // so that we can import from the console
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

lazy val export = project
  .settings(commonSettings:_*)
  .dependsOn(main % "test->test;compile->compile")

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
  ReleaseStep(action = Command.process("publishSigned", _)),
  ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion,
  pushChanges
)
