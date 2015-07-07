package edu.arizona.sista.open

import java.io.File

/**
 * Simple RuleReader
 */
object RuleReader {

  val resourcesDir = "/edu/arizona/sista/odin/domains/open/grammars"
  // For shell
  val resourcesPath = s"src/main/resources$resourcesDir"

  def readRules(rules: String): String =
    readBuiltInRules() +"\n\n" + rules

  // the built-in grammar lives under odin/domains/open/grammar
  def readBuiltInRules(): String = {
    readRuleFilesFromDir(new File(resourcesPath))
  }

  def readRuleFilesFromDir(file: File):String = {
    val ruleFiles =
      file.listFiles
        .filter(f => f.getName.endsWith(".yml"))
    val rules =
      ruleFiles
        .map{case f:File => readFile(f.getAbsolutePath)}
        .mkString("\n")
    rules + "\n"
  }

  def readFile(filename: String) = {
    val source = io.Source.fromFile(filename)
    val data = source.mkString
    source.close()
    data
  }

  def readResource(filename: String): String = {
    val source = io.Source.fromURL(getClass.getResource(filename))
    val data = source.mkString
    source.close()
    data
  }
}
