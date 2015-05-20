name := "reach"

version := "1.0"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

resolvers ++= Seq(
  "BioPAX Releases" at "http://biopax.sourceforge.net/m2repo/releases",
  "BioPAX Snapshots" at "http://biopax.sourceforge.net/m2repo/snapshots"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "edu.arizona.sista" %% "processors" % "5.3-SNAPSHOT",
  "edu.arizona.sista" %% "processors" % "5.3-SNAPSHOT" classifier "models",
  "com.typesafe" % "config" % "1.2.1",
  "commons-io" % "commons-io" % "2.4",
  "org.biopax.paxtools" % "paxtools-core" % "4.3.0",
  "jline" % "jline" % "2.12"
)
