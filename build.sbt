name := "reach"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.0.M6-SNAP17" % "test",
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "edu.arizona.sista" %% "processors" % "4.0-SNAPSHOT",
  "edu.arizona.sista" %% "processors" % "4.0-SNAPSHOT" classifier "models",
  "org.apache.lucene" % "lucene-core" % "4.2.1",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.2.1",
  "org.apache.lucene" % "lucene-queryparser" % "4.2.1",
  "org.apache.lucene" % "lucene-highlighter" % "4.2.1",
  "org.biopax.paxtools" % "paxtools-core" % "4.2.1"
)
