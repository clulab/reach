name := "reach-main"

libraryDependencies ++= {
  val akkaV = "2.5.4"
  val luceVer = "5.3.1"
  val procVer = "7.5.2"

  Seq(
    "ai.lum"              %%  "nxmlreader"  % "0.0.9",
    "commons-io"           %  "commons-io"  % "2.4",
    "jline"                %  "jline"       % "2.12.1",
    "com.typesafe"         %  "config"      % "1.3.1",

    "org.apache.lucene"    %  "lucene-core"              % luceVer,
    "org.apache.lucene"    %  "lucene-analyzers-common"  % luceVer,
    "org.apache.lucene"    %  "lucene-queryparser"       % luceVer,
    "org.biopax.paxtools"  %  "paxtools-core"            % "4.3.1",
    "org.clulab"           %  "bioresources"             % "1.1.24",
    "org.clulab"          %%  "processors-main"          % procVer,
    "org.clulab"          %%  "processors-corenlp"       % procVer,
    "org.clulab"          %%  "processors-modelsmain"    % procVer,
    "org.clulab"          %%  "processors-modelscorenlp" % procVer,
    "org.clulab"          %%  "processors-odin"          % procVer,

    // logging
    "com.typesafe.scala-logging"  %%  "scala-logging"    % "3.7.2",
    "ch.qos.logback"               %  "logback-classic"  % "1.0.10",
    "org.slf4j"                    %  "slf4j-api"        % "1.7.10",

    // AKKA
    "com.typesafe.akka"   %%  "akka-actor"   % akkaV,
//  "com.typesafe.akka"   %%  "akka-remote"  % akkaV,
    "com.typesafe.akka"   %%  "akka-slf4j"   % akkaV,

    // testing
    "org.scalatest"       %%  "scalatest"      % "3.0.1"  % "test",
    "com.typesafe.akka"   %%  "akka-testkit"   % akkaV    % "test",
    "org.clulab" %% "fatdynet" % "0.2.2"
  )

}
