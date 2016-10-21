name := "reach-main"

libraryDependencies ++= {
  val processorsVersion = "6.0.1-SNAPSHOT"
  val luceneVersion = "5.3.1"
  Seq(
    "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    "org.clulab" % "bioresources" % "1.1.17",
    "org.clulab" %% "processors-main" % processorsVersion,
    "org.clulab" %% "processors-corenlp" % processorsVersion,
    "org.clulab" %% "processors-models" % processorsVersion,
    "com.typesafe" % "config" % "1.2.1",
    "commons-io" % "commons-io" % "2.4",
    "org.biopax.paxtools" % "paxtools-core" % "4.3.1",
    "jline" % "jline" % "2.12.1",
    "org.apache.lucene" % "lucene-core" % luceneVersion,
    "org.apache.lucene" % "lucene-analyzers-common" % luceneVersion,
    "org.apache.lucene" % "lucene-queryparser" % luceneVersion,
    "ai.lum" %% "nxmlreader" % "0.0.7",
    // logging
    "ch.qos.logback" %  "logback-classic" % "1.1.7",
    "com.typesafe.scala-logging" %%  "scala-logging" % "3.4.0"
  )
}
