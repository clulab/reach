name := "reach-assembly"

//resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= {
  //val akkaV = "2.4.20"
  //val akkaHttpV = "10.1.12"
  val json4sV = "3.5.2" // Coordinate with processors or expect problems.

  val akkaV = "2.5.4"
  val akkaHttpV = "10.0.9"
  Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "ai.lum" %% "common" % "0.1.4",
    "org.json4s" %% "json4s-jackson" % json4sV,
    // graph-based CSP
    //"org.choco-solver" % "choco-graph" % "3.3.0",
    // AKKA
    "com.typesafe.akka" %% "akka-stream" % akkaV,
    // "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-http" % akkaHttpV,
    "com.typesafe.akka" %% "akka-http-xml" % akkaHttpV
  )
}
