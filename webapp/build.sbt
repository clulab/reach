name := """webapp"""
scalaVersion := "2.11.11"

libraryDependencies ++= Seq(
  //guice,
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
)

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "org.clulab.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "org.clulab.binders._"
