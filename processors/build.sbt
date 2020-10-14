name := "reach-processors"

// needed for processors-models, which is linked from processors-main
resolvers += "Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release"


libraryDependencies ++= {
  val procVer = "8.1.1"

  Seq(
    "com.typesafe"         %  "config"      % "1.3.1",

    "org.clulab"          %%  "processors-main"          % procVer,
    "org.clulab"          %%  "processors-corenlp"       % procVer,
    "org.clulab"          %%  "processors-odin"          % procVer,
    "org.clulab"           %  "bioresources"             % "1.1.34-SNAPSHOT",
    "org.clulab"          %%  "fatdynet"                 % "0.2.5",

    // logging
    "com.typesafe.scala-logging"  %%  "scala-logging"    % "3.7.2",
    "ch.qos.logback"               %  "logback-classic"  % "1.0.10",
    "org.slf4j"                    %  "slf4j-api"        % "1.7.10",

    // testing
    "org.scalatest"       %%  "scalatest"      % "3.0.1"  % "test"
  )
}
