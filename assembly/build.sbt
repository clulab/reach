name := "reach-assembly"

// resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += Resolver.bintrayRepo("hseeberger", "maven")

libraryDependencies ++= {
  val akkaV = "2.5.4"
  val akkaHTTPV = "10.0.9"
  val json4sV = "3.5.2"

  Seq(
    "ai.lum"              %%  "common"      % "0.0.8",
    // "commons-io"           %  "commons-io"  % "2.4",

    // graph-based CSP
    "org.choco-solver"     %  "choco-graph"  % "3.3.0",

    // Twirl
    //"com.typesafe.play"   %%  "twirl-api"    % twirlV,

    // logging
    // "com.typesafe.scala-logging"  %%  "scala-logging"    % "3.7.2",
    // "ch.qos.logback"               %  "logback-classic"  % "1.0.10",
    "org.slf4j"                    %  "slf4j-api"        % "1.7.10",

    // akka
    "com.typesafe.akka"   %%  "akka-actor"         % akkaV,
    "com.typesafe.akka"   %%  "akka-stream"        % akkaV,
    "com.typesafe.akka"   %%  "akka-slf4j"         % akkaV,

    // akka-http
    "com.typesafe.akka"   %%  "akka-http"          % akkaHTTPV,
    "com.typesafe.akka"   %%  "akka-http-core"     % akkaHTTPV,
    "com.typesafe.akka"   %%  "akka-http-xml"      % akkaHTTPV,
    "de.heikoseeberger"   %%  "akka-http-json4s"   % "1.17.0",

    // testing
    "org.scalatest"       %%  "scalatest"          % "3.0.1"    % "test",
    "com.typesafe.akka"   %%  "akka-testkit"       % akkaV      % "test",
    "com.typesafe.akka"   %%  "akka-http-testkit"  % akkaHTTPV  % "test"
  )
}
