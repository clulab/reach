// used for building project website
resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.4")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0") //shows warnings for bad coding style

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.10") //adds quite useful dependencyUpdates task


