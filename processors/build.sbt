name := "reach-processors"

// needed for processors-models, which is linked from processors-main
resolvers += "clulab" at "https://artifactory.clulab.org/artifactory/sbt-release"


libraryDependencies ++= {
  val procVer = "8.5.3"

  Seq(
    "com.typesafe"         %  "config"      % "1.3.1",

    "org.clulab"          %%  "processors-main"          % procVer,
    "org.clulab"          %%  "processors-corenlp"       % procVer,

    "ai.lum" %% "common" % "0.1.4",
    
    // logging
    "com.typesafe.scala-logging"  %%  "scala-logging"    % "3.7.2",
    "ch.qos.logback"               % "logback-classic"   % "1.2.8", // up to 1.2.8; less than 1.2 is vulnerable
    "org.slf4j"                    %  "slf4j-api"        % "1.7.10",

    // testing
    "org.scalatest"       %%  "scalatest"      % "3.0.1"  % "test"
  )
}
