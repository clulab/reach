package org.clulab.reach.indexer

import java.io.File
import java.nio.file.Paths
import java.util.regex.Pattern
import org.clulab.struct.MutableNumber
import org.clulab.utils.{Files, StringUtils}
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.{Document, Field, StoredField, StringField, TextField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.FSDirectory
import ai.lum.nxmlreader.NxmlReader
import org.slf4j.LoggerFactory
import NxmlIndexer._
import scala.collection.mutable
import org.clulab.processors.bionlp.BioNLPProcessor


/**
 * Indexes a bunch of NXML files so we can run quick searches wo/ grep :)
 * User: mihais
 * Date: 10/19/15
 */
class NxmlIndexer {
  def index(docsDir:String, mapFile:String, indexDir:String): Unit = {
    val files = Files.findFiles(docsDir, "nxml")
    logger.info(s"Preparing to index ${files.length} files...")
    val nxmlReader = new NxmlReader(IGNORE_SECTIONS.toSet)
    val fileToPmc = readMapFile(mapFile)

    // check that all files exist in the map
    var failed = false
    var count = 0
    for(file <- files) {
      val fn = getFileName(file, "nxml")
      if(! fileToPmc.contains(fn)) {
        logger.info(s"Did not find map for file $fn!")
        failed = true
        count += 1
      }
    }
    // if(failed) throw new RuntimeException("Failed to map some files. Exiting...")
    if(count > 0)
      logger.info(s"Failed to find PMC id for ${count} files.")

    // index
    val analyzer = new StandardAnalyzer
    val config = new IndexWriterConfig(analyzer)
    val index = FSDirectory.open(Paths.get(indexDir))
    val writer = new IndexWriter(index, config)
    // BioNLPProcessor to preprocess the text
    val processor = new BioNLPProcessor
    count = 0
    for (file <- files) {
      // Preprocess bio text
      val rawText = io.Source.fromFile(file).getLines.mkString("\n")
      // This is potentially incorrect because this preprocesses both text and NXML tags...
      // TODO: this needs to be fixed by adding a preprocessing callback to NxmlReader
      val preprocessedText = processor.preprocessText(rawText)
      // Parse the preprocessed nxml
      val nxmlDoc = nxmlReader.parse(preprocessedText)

      if(nxmlDoc != null && fileToPmc.contains(getFileName(file, "nxml"))) {
        val meta = fileToPmc.get(getFileName(file, "nxml")).get
        val text = nxmlDoc.text
        val nxml = readNxml(file)
        addDoc(writer, meta, text, nxml)
      } else if(nxmlDoc == null) {
        logger.info(s"WARNING: NXML parsing failed on file $file!")
      }
      count += 1
      if(count % 100 == 0)
        logger.info(s"Indexed ${count}/${files.size} files.")

    }
    writer.close()
    logger.info(s"Indexing complete. Indexed ${count}/${files.size} files.")

  }

  def addDoc(writer:IndexWriter, meta:PMCMetaData, text:String, nxml:String): Unit = {
    val d = new Document
    d.add(new TextField("text", text, Field.Store.YES))
    d.add(new StringField("id", meta.pmcId, Field.Store.YES))
    d.add(new StringField("year", meta.year, Field.Store.YES)) // TODO: index as DateField or NumericField?
    d.add(new StoredField("nxml", nxml))
    writer.addDocument(d)
  }

  def readNxml(file:File):String = {
    val os = new StringBuilder
    for(line <- io.Source.fromFile(file).getLines()) {
      os.append(line)
      os.append("\n")
    }
    os.toString()
  }

  def readMapFile(mapFile:String):Map[String, PMCMetaData] = {
    // map from file name (wo/ extension) to pmc id
    val map = new mutable.HashMap[String, PMCMetaData]()
    val errorCount = new MutableNumber[Int](0)
    for(line <- io.Source.fromFile(mapFile).getLines()) {
      val tokens = line.split("\\t")
      if(tokens.length > 2) { // skip headers
      val fn = getFileName(tokens(0), "tar.gz")
        val pmcId = tokens(2)
        val journalName = tokens(1)
        val year = extractPubYear(journalName, errorCount)
        map += fn -> new PMCMetaData(pmcId, year)
        // logger.debug(s"$fn -> $pmcId, $year")
      }
    }
    logger.info(s"PMC map contains ${map.size} files.")
    logger.info(s"Found $errorCount errors when processing this file.")
    map.toMap
  }

  def getFileName(file:File, extension:String):String = getFileName(file.getName, extension)

  def getFileName(path:String, extension:String):String = {
    val slash = path.lastIndexOf(File.separator)
    val ext = path.indexOf("." + extension)
    assert(ext > slash)
    path.substring(slash + 1, ext)
  }

  def extractPubYear(journalName:String, errorCount:MutableNumber[Int]):String = {
    val m = YEAR_PATTERN.matcher(journalName)
    if(m.find()) {
      return m.group(1)
    }
    errorCount.value = errorCount.value + 1
    logger.info(s"WARNING: did not find publication year for journal $journalName!")
    "1950"
  }
}

case class PMCMetaData(
  val pmcId:String,
  val year:String)

object NxmlIndexer {
  val logger = LoggerFactory.getLogger(classOf[NxmlIndexer])
  val IGNORE_SECTIONS = Array("references", "materials", "materials|methods", "methods", "supplementary-material")

  val YEAR_PATTERN = Pattern.compile("\\s+(19|18|20[0-9][0-9])") // \\s+(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)", Pattern.CASE_INSENSITIVE)

  def main(args:Array[String]): Unit = {
    val props = StringUtils.argsToProperties(args)
    val indexDir = props.getProperty("index")
    val docsDir = props.getProperty("docs")
    val mapFile = props.getProperty("map")

    val indexer = new NxmlIndexer
    indexer.index(docsDir, mapFile, indexDir)
  }
}
