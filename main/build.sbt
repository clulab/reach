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
  "com.typesafe.scala-logging" %%  "scala-logging" % "3.4.0"
  
  //breeze:
  // Last stable release
  "org.scalanlp" %% "breeze" % "0.13.1",
  
  // Native libraries are not included by default. add this if you want them (as of 0.7)
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "0.13.1",
  
  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  //"org.scalanlp" %% "breeze-viz" % "0.13.1"
)
