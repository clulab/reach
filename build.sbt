import ReleaseTransformations._

name := "reach-assembly"

organization := "org.clulab"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

testOptions in Test += Tests.Argument("-oD")

parallelExecution in Test := false

// forward sbt's stdin to forked process
connectInput in run := true

// don't show output prefix
outputStrategy := Some(StdoutOutput)

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

// publish to a maven repo
publishMavenStyle := true

// the standard maven repository
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

// let’s remove any repositories for optional dependencies in our artifact
pomIncludeRepository := { _ => false }

// mandatory stuff to add to the pom for publishing
pomExtra := (
  <url>https://github.com/clulab/reach-assembly</url>
  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/clulab/reach-assembly</url>
    <connection>https://github.com/clulab/reach-assembly</connection>
  </scm>
  <developers>
    <developer>
      <id>ghp</id>
      <name>Gus Hahn-Powell</name>
      <email>gushahnpowell@gmail.com</email>
    </developer>
  </developers>)

//
// end publishing settings
//

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.clulab" %% "reach" % "1.3.2",
  "org.clulab" %% "processors-main" % "6.0.0",
  "org.clulab" %% "processors-corenlp" % "6.0.0",
  "org.clulab" %% "processors-models" % "6.0.0",
  "com.typesafe" % "config" % "1.2.1",
  "commons-io" % "commons-io" % "2.4",
  // logging
  "ch.qos.logback" %  "logback-classic" % "1.1.7",
  "com.typesafe.scala-logging" %%  "scala-logging" % "3.4.0"
)

// settings for building project website

site.settings
// include documentation
site.includeScaladoc()

ghpages.settings

git.remoteRepo := "git@github.com:clulab/reach-assembly.git"
