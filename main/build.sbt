name := "reach-main"

libraryDependencies ++= {

  val procVer = "6.0.6-SNAPSHOT"

  Seq(
    "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    "org.clulab" % "bioresources" % "1.1.22",
    "org.clulab" %% "processors-main" % procVer,
    "org.clulab" %% "processors-corenlp" % procVer,
    "org.clulab" %% "processors-models" % procVer,
    "org.clulab" %% "odin" % procVer,
    "commons-io" % "commons-io" % "2.4",
    "org.biopax.paxtools" % "paxtools-core" % "4.3.1",
    "jline" % "jline" % "2.12.1",
    "org.apache.lucene" % "lucene-core" % "5.3.1",
    "org.apache.lucene" % "lucene-analyzers-common" % "5.3.1",
    "org.apache.lucene" % "lucene-queryparser" % "5.3.1",
    "ai.lum" %% "nxmlreader" % "0.0.9",
    // logging
    "ch.qos.logback" %  "logback-classic" % "1.1.7",
    "com.typesafe.scala-logging" %%  "scala-logging" % "3.4.0"
  )

}
