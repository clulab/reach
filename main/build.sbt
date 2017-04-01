name := "reach-main"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.clulab" % "bioresources" % "1.1.19",
  "org.clulab" %% "processors-main" % "6.0.1",
  "org.clulab" %% "processors-corenlp" % "6.0.1",
  "org.clulab" %% "processors-models" % "6.0.1",
  "com.typesafe" % "config" % "1.2.1",
  "commons-io" % "commons-io" % "2.4",
  "org.biopax.paxtools" % "paxtools-core" % "4.3.1",
  "jline" % "jline" % "2.12.1",
  "org.apache.lucene" % "lucene-core" % "5.3.1",
  "org.apache.lucene" % "lucene-analyzers-common" % "5.3.1",
  "org.apache.lucene" % "lucene-queryparser" % "5.3.1",
  "ai.lum" %% "nxmlreader" % "0.0.7",
  // logging
  "ch.qos.logback" %  "logback-classic" % "1.1.7",
  "com.typesafe.scala-logging" %%  "scala-logging" % "3.4.0",
  "org.scala-graph" %% "graph-core" % "1.11.3",
  "org.apache.tinkerpop" % "gremlin-core" % "3.2.4",
  "org.apache.tinkerpop" % "tinkergraph-gremlin" % "3.2.4",
  "com.michaelpollmeier" %% "gremlin-scala" % "3.2.4.1",
  "org.xerial" % "sqlite-jdbc" % "3.16.1",
  "org.scalanlp" %% "breeze" % "0.13",
  "org.scalanlp" %% "breeze-natives" % "0.13",
  "org.scalanlp" %% "breeze-viz" % "0.13",
  "org.jfree" % "jfreechart" % "1.0.19"
)
