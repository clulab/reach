name := "reach"

version := "1.0"

organization := "edu.arizona.sista"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

// fork jvm to separate process
fork := true

// options for forked jvm
javaOptions += "-Xmx6G"

// forward sbt's stdin to forked process
connectInput in run := true

// don't show output prefix
outputStrategy := Some(StdoutOutput)

//
// publishing settings
//

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
  </developers>)

//
// end publishing settings
//

resolvers ++= Seq(
  "BioPAX Releases" at "http://biopax.sourceforge.net/m2repo/releases",
  "BioPAX Snapshots" at "http://biopax.sourceforge.net/m2repo/snapshots"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "edu.arizona.sista" %% "processors" % "5.4-SNAPSHOT",
  "edu.arizona.sista" %% "processors" % "5.4-SNAPSHOT" classifier "models",
  "com.typesafe" % "config" % "1.2.1",
  "commons-io" % "commons-io" % "2.4",
  "org.biopax.paxtools" % "paxtools-core" % "4.3.0",
  "jline" % "jline" % "2.12.1"
)
