package org.clulab.processors

import org.clulab.processors.bionlp.BioNLPProcessor
import org.scalatest._

import scala.io.Source

/**
 * Tests BioNLPProcessor on the processing of an actual paper
 * User: mihais
 * Date: 11/16/14
 */
class TestBioNLPProcessor2 extends FlatSpec with Matchers {
  var proc:Processor = new BioNLPProcessor()

  /**
    * Read contents of rule file in the classpath, given some path
    *
    * @param path the path to a file
    * @return file contents as a String
    */
  def readFile(path: String) = {
    val stream = getClass.getClassLoader.getResourceAsStream(path)
    val source = Source.fromInputStream(stream)
    val data = source.mkString
    source.close()
    data
  }

  "BioNLPProcessor" should "parse abstract text" in {
    val text = readFile("org/clulab/processors/PLoS_One_2013_Dec_18_8_12_e84604.abstract.txt")
    val doc = proc.annotate(text)
    //println(s"Generated a doc with ${doc.sentences.size} sentences.")
    doc.sentences.size should be (7)
    doc.sentences(0).syntacticTree.isDefined should be (true)
  }

  it should "parse body text" in {
    val text = readFile("org/clulab/processors/PLoS_One_2013_Dec_18_8_12_e84604.body.txt")
    val doc = annotate(text)
    doc.sentences(0).syntacticTree.isDefined should be (true)
  }

  def annotate(text:String):Document = {
    val doc = proc.mkDocument(text, keepText = false)
    //println(s"Processing a document with ${doc.sentences.size} sentences...")
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)
    proc.parse(doc)
    doc.clear()
    doc
  }

}
