package edu.arizona.sista.reach.nxml.indexer

import java.io.File
import java.nio.file.Paths
import java.util.regex.{Matcher, Pattern}

import edu.arizona.sista.reach.nxml.{FriesEntry, NxmlReader}
import edu.arizona.sista.utils.{Files, StringUtils}
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.{StringField, Field, TextField, Document, StoredField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.FSDirectory
import org.slf4j.LoggerFactory
import NxmlIndexer._

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
 * Indexes a bunch of NXML files so we can run quick searches wo/ grep :)
 * User: mihais
 * Date: 10/19/15
 */
class NxmlIndexer {
  def index(docsDir:String, mapFile:String, indexDir:String): Unit = {
    val files = Files.findFiles(docsDir, "nxml")
    logger.debug(s"Preparing to index ${files.length} files...")
    val nxmlReader = new NxmlReader(IGNORE_SECTIONS)
    val fileToPmc = readMapFile(mapFile)

    // check that all files exist in the map
    var failed = false
    var count = 0
    for(file <- files) {
      val fn = getFileName(file, "nxml")
      if(! fileToPmc.contains(fn)) {
        logger.debug(s"Did not find map for file $fn!")
        failed = true
        count += 1
      }
    }
    // if(failed) throw new RuntimeException("Failed to map some files. Exiting...")
    if(count > 0)
      logger.debug(s"Failed to find PMC id for $count files.")

    // index
    val analyzer = new StandardAnalyzer
    val config = new IndexWriterConfig(analyzer)
    val index = FSDirectory.open(Paths.get(indexDir))
    val writer = new IndexWriter(index, config)
    count = 0
    for (file <- files) {
      val entries = Try(nxmlReader.readNxml(file)) match {
        case Success(v) => v
        case Failure(e) =>
          logger.debug(s"NxmlReader failed on file $file")
          Nil
      }
      if(entries != Nil && fileToPmc.contains(getFileName(file, "nxml"))) {
        val meta = fileToPmc.get(getFileName(file, "nxml")).get
        val text = mergeEntries(entries)
        val nxml = readNxml(file)
        addDoc(writer, meta, text, nxml)
      }
      count += 1
      if(count % 100 == 0)
        logger.debug(s"Indexed $count/${files.size} files.")

    }
    writer.close()
    logger.debug(s"Indexing complete. Indexed $count/${files.size} files.")

  }

  def addDoc(writer:IndexWriter, meta:PMCMetaData, text:String, nxml:String): Unit = {
    val d = new Document
    d.add(new TextField("text", text, Field.Store.YES))
    d.add(new StringField("id", meta.pmcId, Field.Store.YES))
    d.add(new StringField("year", meta.year, Field.Store.YES))
    d.add(new StoredField("nxml", nxml))
    writer.addDocument(d)
  }

  def mergeEntries(entries:Seq[FriesEntry]):String = {
    val os = new StringBuilder()
    entries.foreach(e => os.append(e.text + "\n"))
    os.toString()
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
    for(line <- io.Source.fromFile(mapFile).getLines()) {
      val tokens = line.split("\\t")
      if(tokens.length > 2) { // skip headers
        val fn = getFileName(tokens(0), "tar.gz")
        val pmcId = tokens(2)
        val journalName = tokens(1)
        val year = extractPubYear(journalName)
        map += fn -> new PMCMetaData(pmcId, year)
        logger.debug(s"$fn -> $pmcId, $year")
      }
    }
    logger.debug(s"PMC map contains ${map.size} files.")
    map.toMap
  }

  def getFileName(file:File, extension:String):String = getFileName(file.getName, extension)

  def getFileName(path:String, extension:String):String = {
    val slash = path.lastIndexOf(File.separator)
    val ext = path.indexOf("." + extension)
    assert(ext > slash)
    path.substring(slash + 1, ext)
  }

  def extractPubYear(journalName:String):String = {
    val m = YEAR_PATTERN.matcher(journalName)
    if(m.find()) {
      return m.group(1)
    }
    throw new RuntimeException(s"ERROR: did not find publication year for journal $journalName!")
  }
}

case class PMCMetaData(
  val pmcId:String,
  val year:String)

object NxmlIndexer {
  val logger = LoggerFactory.getLogger(classOf[NxmlIndexer])
  val IGNORE_SECTIONS = Array("references", "materials", "materials|methods", "methods", "supplementary-material")

  val YEAR_PATTERN = Pattern.compile("\\s+(19|20[0-9][0-9])\\s+(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)", Pattern.CASE_INSENSITIVE)

  def main(args:Array[String]): Unit = {
    val props = StringUtils.argsToProperties(args)
    val indexDir = props.getProperty("index")
    val docsDir = props.getProperty("docs")
    val mapFile = props.getProperty("map")

    val indexer = new NxmlIndexer
    indexer.index(docsDir, mapFile, indexDir)
  }
}
