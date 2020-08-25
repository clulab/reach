name := "reach-assembly"

//resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= {
  val akkaV = "2.4.3"
  Seq(
    "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    "ai.lum" %% "common" % "0.0.5",
    // graph-based CSP
    //"org.choco-solver" % "choco-graph" % "3.3.0",
    // Twirl
    //"com.typesafe.play"                  %% "twirl-api"                              % twirlV,
    // AKKA
    "com.typesafe.akka"                  %%  "akka-actor"                            % akkaV,
    "com.typesafe.akka"                  %%  "akka-stream"                           % akkaV,
    "com.typesafe.akka"                  %%  "akka-http-experimental"                % akkaV,
    "com.typesafe.akka"                  %%  "akka-http-spray-json-experimental"     % akkaV,
    "com.typesafe.akka"                  %%  "akka-http-testkit"                     % akkaV,
    "com.typesafe.akka"                  %%  "akka-actor"                            % akkaV,
    "com.typesafe.akka"                  %%  "akka-testkit"                          % akkaV    % "test",
    "com.typesafe.akka"                  %%  "akka-slf4j"                            % akkaV,
    "com.typesafe.akka"                  %%  "akka-http-xml-experimental"            % akkaV
  )
}