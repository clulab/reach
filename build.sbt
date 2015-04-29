name := "reach"

version := "1.0"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

resolvers ++= Seq(
  "BioPAX Releases" at "http://biopax.sourceforge.net/m2repo/releases",
  "BioPAX Snapshots" at "http://biopax.sourceforge.net/m2repo/snapshots"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0-SNAP4" % "test",
  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "edu.arizona.sista" %% "processors" % "5.2-SNAPSHOT",
  "edu.arizona.sista" %% "processors" % "5.2-SNAPSHOT" classifier "models",
  "org.apache.lucene" % "lucene-core" % "4.2.1",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.2.1",
  "org.apache.lucene" % "lucene-queryparser" % "4.2.1",
  "org.apache.lucene" % "lucene-highlighter" % "4.2.1",
  "org.biopax.paxtools" % "paxtools-core" % "4.3.0",
  "jline" % "jline" % "2.12"
)
