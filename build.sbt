import ReleaseTransformations._

lazy val commonSettings = Seq(

  organization := "org.clulab",

  scalaVersion := "2.11.11",

  scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation"),

  testOptions in Test += Tests.Argument("-oD"),

  parallelExecution in Global := false,

  // publish to a maven repo
  publishMavenStyle := true,

  // the standard maven repository
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },

  // let’s remove any repositories for optional dependencies in our artifact
  pomIncludeRepository := { _ => false },

  // mandatory stuff to add to the pom for publishing
  pomExtra :=
    <url>https://github.com/clulab/reach</url>
    <licenses>
      <license>
        <name>Apache License, Version 2.0</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>https://github.com/clulab/reach</url>
      <connection>https://github.com/clulab/reach</connection>
    </scm>
    <developers>
      <developer>
        <id>mihai.surdeanu</id>
        <name>Mihai Surdeanu</name>
        <email>mihai@surdeanu.info</email>
      </developer>
    </developers>

  //
  // end publishing settings
  //

)

lazy val dockerSettings = Seq(
  dockerfile in docker := {
    val targetDir = "/app"
    // the assembly task generates a fat jar
    val artifact: File = assembly.value
    val artifactTargetPath = s"$targetDir/${artifact.name}"
    val dockerConf = "docker.conf"
    new Dockerfile {
      from("openjdk")
      add(artifact, artifactTargetPath)
      copy(new File(dockerConf), file(s"$targetDir/$dockerConf"))
      entryPoint("java", "-Dpidfile.path=/dev/null", s"-Dconfig.file=$targetDir/$dockerConf", "-jar", artifactTargetPath)
    }
  },
  imageNames in docker := Seq(
    // sets the latest tag
    ImageName(s"${organization.value}/${name.value}:latest"),
    // sets a name with a tag that contains the project version
    ImageName(
      namespace = Some(organization.value),
      repository = name.value,
      tag = Some("v" + version.value)
    )
  )
)

lazy val root = (project in file("."))
  .enablePlugins(sbtdocker.DockerPlugin)
  .settings(commonSettings)
  .settings(dockerSettings)
  .aggregate(main, causalAssembly, export)
  .dependsOn(main % "test->test;compile", causalAssembly, export) // so that we can import from the console
  .settings(
    name := "reach-exe",
    aggregate in test := false,
    aggregate in assembly := false,
    test in assembly := {},
    mainClass in assembly := {
      val sysMain = System.getProperty("mainClass")
      Option(if (sysMain != null) sysMain else "org.clulab.reach.RunReachCLI")
    },
    assemblyJarName in assembly := s"reach-${version.value}-FAT.jar"
  )

lazy val main = project
  .settings(commonSettings:_*)

lazy val causalAssembly = project.in(file("assembly"))
  .settings(commonSettings:_*)
  .dependsOn(main % "test->test;compile->compile")

lazy val export = project
  .settings(commonSettings:_*)
  .dependsOn(main % "test->test;compile->compile", causalAssembly % "test;compile") // need access to assembly/src/resources


// add a command to dockerize
/* addCommandAlias("dockerize", ";clean;compile;test;docker") */
addCommandAlias("dockerize", ";clean;compile;docker")

//
// publishing settings
//

// these are the steps to be performed during release
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  ReleaseStep(action = Command.process("publishSigned", _)),
  ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion,
  pushChanges
)

// settings for building project website

site.settings
// include documentation
site.includeScaladoc()

ghpages.settings

git.remoteRepo := "git@github.com:clulab/reach.git"
