name := "reach-export"

resolvers += Resolver.bintrayRepo("hseeberger", "maven")

libraryDependencies ++= {
  val akkaV = "2.5.3"
  val akkaHTTPV = "10.0.9"
  val json4sV = "3.5.0"

  Seq(
    "com.typesafe"         %  "config"             % "1.3.0",
    "org.json4s"          %%  "json4s-native"      % json4sV,
    "org.json4s"          %%  "json4s-jackson"     % json4sV,

    // akka
    "com.typesafe.akka"   %%  "akka-actor"         % akkaV,
    "com.typesafe.akka"   %%  "akka-stream"        % akkaV,
    "com.typesafe.akka"   %%  "akka-slf4j"         % akkaV,

    // akka-http
    "com.typesafe.akka"   %%  "akka-http"          % akkaHTTPV,
    "com.typesafe.akka"   %%  "akka-http-core"     % akkaHTTPV,
    "com.typesafe.akka"   %%  "akka-http-xml"      % akkaHTTPV,
    "de.heikoseeberger"   %%  "akka-http-json4s"   % "1.14.0",

    // testing
    "org.scalatest"       %%  "scalatest"          % "2.2.4"    % "test",
    "com.typesafe.akka"   %%  "akka-testkit"       % akkaV      % "test",
    "com.typesafe.akka"   %%  "akka-http-testkit"  % akkaHTTPV  % "test",

    // logging
    "com.typesafe.scala-logging"  %%  "scala-logging"    % "3.4.0",
    "ch.qos.logback"               %  "logback-classic"  % "1.1.7",
    "org.slf4j"                    %  "slf4j-api"        % "1.7.10"
  )

}
