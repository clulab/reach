package edu.arizona.sista.bionlp.reach.indexer

import java.io.{StringReader, BufferedReader}
import java.util

import edu.arizona.sista.processors.Sentence
import edu.arizona.sista.struct.{DirectedGraph, Tree}

import scala.collection.mutable.ArrayBuffer

/**
 * Reads pre-processed sentences produced by REACHToolkit
 * User: mihais
 * Date: 11/5/14
 */
class ReachToolkitReader {
  def readSentences(tagsString:String,
                    parsesString:String,
                    dependenciesString:String):Array[Sentence] = {
    val sentences = readTokensAndTags(tagsString)
    readParse(sentences, parsesString)
    readDeps(sentences, dependenciesString)

    // TODO: READ GUS's NER TAGS HERE!

    sentences
  }

  def readTokensAndTags(text:String):Array[Sentence] = {
    val sentences = new ArrayBuffer[Sentence]()
    val b = new BufferedReader(new StringReader(text))
    // sentences are separated by empty lines
    var done = false
    var sent = new ArrayBuffer[Array[String]]()
    while(! done) {
      var line = b.readLine()
      if(line == null) {
        done = true
      } else {
        line = line.trim
        if(line.isEmpty) {
          val s = mkSentence(sent.toArray)
          if(s.words.length > 0) // sometimes there are multiple empty lines in the .tags file; skip them
            sentences += s
          sent = new ArrayBuffer[Array[String]]()
        } else {
          sent += line.split("\\s+")
        }
      }
    }
    sentences.toArray
  }

  def mkSentence(tokens:Array[Array[String]]):Sentence = {
    val words = new Array[String](tokens.length)
    for(i <- 0 until tokens.length) words(i) = tokens(i)(0)

    val lemmas = new Array[String](tokens.length)
    for(i <- 0 until tokens.length) lemmas(i) = tokens(i)(1).toLowerCase

    val tags = new Array[String](tokens.length)
    for(i <- 0 until tokens.length) tags(i) = tokens(i)(2)

    val chunks = new Array[String](tokens.length)
    for(i <- 0 until tokens.length) chunks(i) = tokens(i)(3)

    val entities = new Array[String](tokens.length)
    for(i <- 0 until tokens.length) entities(i) = tokens(i)(4)

    new Sentence(words, mkStartOffsets(words), mkEndOffsets(words),
      Some(tags), Some(lemmas), Some(entities), None, Some(chunks), None, None)
  }

  def mkStartOffsets(words:Array[String]):Array[Int] = {
    val offsets = new Array[Int](words.length)
    util.Arrays.fill(offsets, -1) // TODO: fill in with proper word start offsets in the document!
    offsets
  }

  def mkEndOffsets(words:Array[String]):Array[Int] = {
    val offsets = new Array[Int](words.length)
    util.Arrays.fill(offsets, -1) // TODO: fill in with proper word end offsets in the document!
    offsets
  }

  def readParse(sentences:Array[Sentence], parsesText:String) {
    val b = new BufferedReader(new StringReader(parsesText))
    // one parse tree per line; no empty lines
    for(sent <- sentences) {
      val l = b.readLine()
      assert(l != null)
      sent.syntacticTree = Some(Tree.mkTree(l))
    }
  }

  def readDeps(sentences:Array[Sentence], depsText:String) {
    val b = new BufferedReader(new StringReader(depsText))
    // each set of deps for one sentence separated by an empty line
    for(sent <- sentences) {
      val deps = readDepsForOneSentence(b)
      assert(deps != null)
      sent.dependencies = Some(DirectedGraph.mkGraph(deps))
    }
  }

  def readDepsForOneSentence(b:BufferedReader):Array[String] = {
    val db = new ArrayBuffer[String]()
    var done = false
    while(! done) {
      var l = b.readLine()
      assert(l != null)
      l = l.trim
      if(l.isEmpty) {
        done = true
      } else {
        db += l
      }
    }
    db.toArray
  }
}
