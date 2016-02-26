name := "reach"

version := "1.2.3-SNAPSHOT"

organization := "org.clulab"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

testOptions in Test += Tests.Argument("-oD")

// fork jvm to separate process
fork := true

parallelExecution in Test := false

// options for forked jvm
javaOptions += "-Xmx5G"

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
  "org.clulab" % "bioresources" % "1.1.1",
  "org.clulab" %% "processors" % "5.8.1",
  "org.clulab" %% "processors" % "5.8.1" classifier "models",
  "com.typesafe" % "config" % "1.2.1",
  "commons-io" % "commons-io" % "2.4",
  "org.biopax.paxtools" % "paxtools-core" % "4.3.0",
  "jline" % "jline" % "2.12.1",
  "org.apache.lucene" % "lucene-core" % "5.3.1",
  "org.apache.lucene" % "lucene-analyzers-common" % "5.3.1",
  "org.apache.lucene" % "lucene-queryparser" % "5.3.1"
)
