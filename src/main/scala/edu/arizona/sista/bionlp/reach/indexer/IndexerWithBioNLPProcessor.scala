package edu.arizona.sista.bionlp.reach.indexer

import java.io._

import edu.arizona.sista.processors.{DocumentSerializer, Document}
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.utils.StringUtils
import edu.arizona.sista.utils.StringUtils._
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.document.{FieldType, Field, StringField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.SimpleFSDirectory
import org.apache.lucene.util.Version
import org.slf4j.LoggerFactory
import scala.sys.process._

import IndexerWithBioNLPProcessor._

import scala.collection.parallel.ForkJoinTaskSupport

/**
 * Creates a Lucene index from a directory containing a collection of papers, using BioNLPProcessor
 * User: mihais
 * Date: 11/5/14
 */
class IndexerWithBioNLPProcessor (
																	 val toolkitDirectory:String,
																	 val nCores:Int = 1) extends Indexer {

	val indexSynchronizer = new Object
	var errorCounter:Counter[String] = null
	var docCount = 0
	var paperCount = 0

  val processor = new BioNLPProcessor()

	/**
	 * Indexes all .nxml files in paperDirectory to a Lucene index stored in indexDirectory; File names are mapped to ids using idFile.
	 */
	override def index(paperDirectory: String, indexDirectory: String, idFile: String): Unit = {
		val nameToId = loadMappings(idFile)
		docCount = 0
		paperCount = 0
		errorCounter = new Counter[String]()

		// very important: use white space tokenization to keep the tokenization produced by BioNLPProcessor
		val analyzer = new WhitespaceAnalyzer(IndexerWithReachToolkit.VERSION)

		// store the index here
		val dir = new SimpleFSDirectory(new File(indexDirectory))

		val config = new IndexWriterConfig(IndexerWithReachToolkit.VERSION, analyzer)
		config.setOpenMode(IndexWriterConfig.OpenMode.CREATE) // overwrite the existing index
		val indexWriter = new IndexWriter(dir, config)

		val papers = findFiles(paperDirectory, EXT)
		logger.info(s"Found ${papers.size} papers to index. Starting to index...")

		val size = papers.size
    logger.info(s"Using $nCores cores for indexing.")

    if(nCores > 1) {
      val parPapers = papers.par
      parPapers.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(nCores))
      parPapers.map(addToIndex(indexWriter, _, size, nameToId))
    } else {
      papers.map(addToIndex(indexWriter, _, size, nameToId))
    }

		indexWriter.close()
		logger.info(s"Indexed $docCount documents for ${papers.size} papers.")
	}

	def addToIndex(indexWriter:IndexWriter, paper:File, size:Int, nameToId:Map[String, String]) {
		val prefix = extractPrefix(paper, EXT)
		val dir = paper.getParent
		runToolkit(paper)

		val paperId = fetchPaperId(prefix, nameToId)
		addDocument(indexWriter, dir, prefix, paperId, "abstract")
		addDocument(indexWriter, dir, prefix, paperId, "body")

		indexSynchronizer.synchronized {
			paperCount += 1
			if (paperCount % 10 == 0)
				logger.info(s"Indexed $paperCount/$size papers. Found the following errors: $errorCounter.")
		}
	}

	def runToolkit(paper:File) {
		val cmd = s"$toolkitDirectory/nxml_to_sections.sh ${paper.getAbsolutePath} ${paper.getParent}"
		logger.debug(s"Running command: $cmd")
		val exitCode = cmd.!
		logger.debug(s"Exit code = $exitCode")
		if(exitCode != 0) {
			// TODO: is this the best way to handle pipeline errors?
			throw new RuntimeException(s"ERROR: nxml_to_sections.sh failed on paper ${paper.getAbsolutePath}!")
		}
	}

	def annotate(text:String): Document = {
		val doc = processor.mkDocument(text, true)
		processor.tagPartsOfSpeech(doc)
		processor.lemmatize(doc)
		processor.recognizeNamedEntities(doc)
		processor.parse(doc)
		doc.clear()
		doc
	}

  def annotateAndSerialize(fn:String):Document = {
    val sfn = fn + ".ser"
    val serializer = new DocumentSerializer
    if(new File(sfn).exists()) {
      // this file has already been processed; reuse that annotation
      val r = new BufferedReader(new FileReader(sfn))
      val annotation = serializer.load(r)
      r.close()
      annotation
    } else {
      logger.debug("Starting BioNLPProcessor on this file: " + fn)
      val annotation = annotate(textFileToString(fn))
      val os = new PrintWriter(new FileWriter(sfn))
      serializer.save(annotation, os)
      os.close()
      annotation
    }
  }

	def addDocument(indexWriter:IndexWriter,
									paperDir:String,
									paperPrefix:String,
									paperId:String,
									section:String) {
		try {
			val doc = new org.apache.lucene.document.Document
      val ser = new DocumentSerializer

			// some meta data; StringField = indexed, but not tokenized
			doc.add(new StringField(PAPERID, paperId, Field.Store.YES))
			doc.add(new StringField(SECTION, section, Field.Store.YES))

			// run BioNLPProcessor on this text
			val annotation = annotateAndSerialize(mkFileName(paperDir, paperPrefix, section, "txt"))

      // we are indexing the tokens, using the provided tokenization (i.e., using WhitespaceAnalyzer here)
      val textFieldType = new FieldType()
      textFieldType.setIndexed(true)
      textFieldType.setStored(true)
      textFieldType.setTokenized(true)
      doc.add(new Field(TEXT, mkTextFromTokens(annotation), textFieldType))

      // all other fields are stored, but not searchable; we will convert them to BioDocument during search
      val otherTextFieldType = new FieldType()
      otherTextFieldType.setIndexed(false)
      otherTextFieldType.setTokenized(false)
      otherTextFieldType.setStored(true)
      doc.add(new Field(ANNOTATION, ser.save(annotation), otherTextFieldType))

			indexSynchronizer.synchronized {
				indexWriter.addDocument(doc)
				docCount += 1
			}
		} catch {
			case e: FileNotFoundException => {
				indexSynchronizer.synchronized {
					errorCounter.incrementCount(section)
					logger.error(s"ERROR: Failed to find section $section for paper $paperDir/$paperPrefix")
					e.printStackTrace()
				}
			}
			case e: Exception => throw e
		}
	}

  def mkTextFromTokens(doc:Document):String = {
    val b = new StringBuilder
    for(s <- doc.sentences) {
      b.append(s.words.mkString(" ")) // this is just for search; it's ok to not respect character offsets here
      b.append(" ")
    }
    b.toString()
  }
}

object IndexerWithBioNLPProcessor {
	val EXT = "nxml"

	val PAPERID = "paperid"
	val SECTION = "section"
	val TEXT = "text"
	val ANNOTATION = "annotation"

	val VERSION = Version.LUCENE_42

	val logger = LoggerFactory.getLogger(classOf[IndexerWithBioNLPProcessor])

	/**
	 * Calls the Indexer on a collection of papers
	 * For example:
	 * scripts/run edu.arizona.sista.bionlp.reach.indexer.Indexer -paper.dir <DIR WITH NXML PAPERS> -index.dir <WHERE LUCENE INDEX WILL BE STORED> -toolkit.dir <WHERE REACHToolkit IS INSTALLED> -id.file <FILE WITH MAPPING FROM FILE NAMES TO PUBMED IDS>
	 *   or
	 * scripts/run edu.arizona.sista.bionlp.reach.indexer.Indexer -props src/main/resources/edu/arizona/sista/bionlp/reach/indexer/indexer.properties
	 * Notes:
	 * - all directories must be provided as absolute paths (there are some `cd` commans in here that mess up relative paths).
	 * - paper.dir may contain subdirectories of any depth with papers (e.g., separated by journal)
	 * - toolkit files are stored in the same location as the original paper under paper.dir
	 * - existing toolkit files are reused (i.e., if a paper has been pre-processed, we won't do it again)
	 */
	def main(args: Array[String]) {
		val props = argsToProperties(args)
		val paperDirectory = props.getProperty("paper.dir")
		assert(paperDirectory != null)
		val indexDirectory = props.getProperty("index.dir")
		assert(indexDirectory != null)
		val toolkitDirectory = props.getProperty("toolkit.dir")
		assert(toolkitDirectory != null)
		val idFile = props.getProperty("id.file")
		assert(idFile != null)
		val nCores = StringUtils.getInt(props, "cores", 1)

		val indexer = new IndexerWithBioNLPProcessor(toolkitDirectory, nCores)
		indexer.index(paperDirectory, indexDirectory, idFile)
	}
}
