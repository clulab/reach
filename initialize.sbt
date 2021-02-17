// Certain library dependencies, particularly Stanford NLP, have been observed to have problems with
// versions of Java other than the required one.  Proceed with caution if you ignore this check.
// See https://stackoverflow.com/questions/19208942/enforcing-java-version-for-scala-project-in-sbt
initialize in ThisBuild := {
  val _ = initialize.value // Run the previous initialization.
  val required = "1.8"
  val current  = sys.props("java.specification.version")
  val approved = current == required

  // To stop sbt in its tracks, make this assumption.
  // assume(approved)
  // or otherwise just log the situation.
  if (approved)
    sLog.value.info(s"Java $current was detected and approved.")
  else
    sLog.value.error(s"Unsupported Java version: Eidos requires $required but found $current instead.")
}
