name := "reach-main"

libraryDependencies ++= {
  val luceVer = "5.3.1"

  Seq(
    "ai.lum"              %%  "nxmlreader"  % "0.1.2",
    "commons-io"           %  "commons-io"  % "2.4",
    "jline"                %  "jline"       % "2.12.1",
    "com.typesafe"         %  "config"      % "1.3.1",

    "org.apache.lucene"    %  "lucene-core"              % luceVer,
    "org.apache.lucene"    %  "lucene-analyzers-common"  % luceVer,
    "org.apache.lucene"    %  "lucene-queryparser"       % luceVer,
    "org.biopax.paxtools"  %  "paxtools-core"            % "4.3.1",

    // logging
    "com.typesafe.scala-logging"  %%  "scala-logging"    % "3.7.2",
    "ch.qos.logback"               %  "logback-classic"  % "1.0.10",
    "org.slf4j"                    %  "slf4j-api"        % "1.7.10",

    // testing
    "org.scalatest"       %%  "scalatest"      % "3.0.1"  % "test"
  )

}
