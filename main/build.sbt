name := "reach-main"

libraryDependencies ++= {
  val akkaV = "2.5.3"
  val luceVer = "5.3.1"
  val procVer = "6.1.2-sri"

  Seq(
    "ai.lum"              %%  "nxmlreader"  % "0.0.9",
    "commons-io"           %  "commons-io"  % "2.4",
    "jline"                %  "jline"       % "2.12.1",

    "org.apache.lucene"    %  "lucene-core"              % luceVer,
    "org.apache.lucene"    %  "lucene-analyzers-common"  % luceVer,
    "org.apache.lucene"    %  "lucene-queryparser"       % luceVer,
    "org.biopax.paxtools"  %  "paxtools-core"            % "4.3.1",
    "org.clulab"           %  "bioresources"             % "1.1.25-sri",
    "org.clulab"          %%  "processors-main"          % procVer,
    "org.clulab"          %%  "processors-corenlp"       % procVer,
    "org.clulab"          %%  "processors-models"        % procVer,
    "org.clulab"          %%  "processors-odin"          % procVer,

    // logging
    "com.typesafe.scala-logging"  %%  "scala-logging"    % "3.4.0",
    "ch.qos.logback"               %  "logback-classic"  % "1.1.7",
    "org.slf4j"                    %  "slf4j-api"        % "1.7.10",

    // AKKA
    "com.typesafe.akka"   %%  "akka-actor"   % akkaV,
//  "com.typesafe.akka"   %%  "akka-remote"  % akkaV,
    "com.typesafe.akka"   %%  "akka-slf4j"   % akkaV,

    // testing
    "org.scalatest"       %%  "scalatest"      % "2.2.4"  % "test",
    "com.typesafe.akka"   %%  "akka-testkit"   % akkaV    % "test"
  )

}
