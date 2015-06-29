name := "reach"

version := "1.0"

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
