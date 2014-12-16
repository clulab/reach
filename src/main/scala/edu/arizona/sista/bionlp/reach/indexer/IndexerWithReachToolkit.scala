package edu.arizona.sista.bionlp.reach.indexer

import java.io._

import edu.arizona.sista.struct.Counter
import edu.arizona.sista.utils.StringUtils
import edu.arizona.sista.utils.StringUtils._
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.document.{FieldType, Field, StringField}
import org.apache.lucene.index.{IndexWriterConfig, IndexWriter}
import org.apache.lucene.store.SimpleFSDirectory
import org.apache.lucene.util.Version
import org.slf4j.LoggerFactory
import scala.collection.parallel.ForkJoinTaskSupport
import scala.sys.process._
import IndexerWithReachToolkit._

/**
 * Creates a Lucene index from a directory containing a collection of papers, using REACHToolkit
 * The files produced by the toolkit are stored in the same location as the original paper; existing files are reused
 * User: mihais
 * Date: 9/12/14
 */
class IndexerWithReachToolkit (val toolkitDirectory:String,
															 val nCores:Int = 1) extends Indexer {

	val indexSynchronizer = new Object
	var errorCounter:Counter[String] = null
	var docCount = 0
	var paperCount = 0

	def index(paperDirectory:String,
						indexDirectory:String,
						idFile:String) {

		val nameToId = loadMappings(idFile)
		docCount = 0
		paperCount = 0
		errorCounter = new Counter[String]()

		// very important: use white space tokenization to keep the tokenization produced by REACHToolkit
		val analyzer = new WhitespaceAnalyzer(IndexerWithReachToolkit.VERSION)

		// store the index here
		val dir = new SimpleFSDirectory(new File(indexDirectory))

		val config = new IndexWriterConfig(IndexerWithReachToolkit.VERSION, analyzer)
		config.setOpenMode(IndexWriterConfig.OpenMode.CREATE) // overwrite the existing index
		val indexWriter = new IndexWriter(dir, config)

		val papers = findFiles(paperDirectory, EXT)
		logger.info(s"Found ${papers.size} papers to index. Starting to index...")

		val size = papers.size
		val parPapers = papers.par
		parPapers.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(nCores))
		logger.info(s"Using $nCores cores for indexing.")
		papers.map(addToIndex(indexWriter, _, size, nameToId))

		indexWriter.close()
		logger.info(s"Indexed $docCount documents for ${papers.size} papers.")
	}

	def addToIndex(indexWriter:IndexWriter, paper:File, size:Int, nameToId:Map[String, String]) {
		val prefix = extractPrefix(paper, EXT)
		val dir = paper.getParent
		if(toolkitOutputExists(dir, prefix)) {
			logger.debug(s"Toolkit output exists for prefix $dir/$prefix. Skipping it.")
		} else {
			runToolkit(paper)
		}

		val paperId = fetchPaperId(prefix, nameToId)
		addDocument(indexWriter, dir, prefix, paperId, "abstract")
		addDocument(indexWriter, dir, prefix, paperId, "body")

		indexSynchronizer.synchronized {
			paperCount += 1
			if (paperCount % 10 == 0)
				logger.info(s"Indexed $paperCount/$size papers. Found the following errors: $errorCounter.")
		}
	}

	def toolkitOutputExists(dir:String, prefix:String):Boolean = {
		val f = new File(dir + File.separator + prefix + ".txt")
		f.exists()
	}

	def addDocument(indexWriter:IndexWriter,
									paperDir:String,
									paperPrefix:String,
									paperId:String,
									section:String) {
		try {
			val doc = new org.apache.lucene.document.Document

			// some meta data; StringField = indexed, but not tokenized
			doc.add(new StringField(PAPERID, paperId, Field.Store.YES))
			doc.add(new StringField(SECTION, section, Field.Store.YES))

			// we are indexing the tokens, using the provided tokenization (i.e., using WhitespaceAnalyzer here)
			val textFieldType = new FieldType()
			textFieldType.setIndexed(true)
			textFieldType.setStored(true)
			textFieldType.setTokenized(true)
			doc.add(new Field(TOK, textFileToString(mkFileName(paperDir, paperPrefix, section, "tok")), textFieldType))

			// all other fields are stored, but not searchable; we will convert them to BioDocument during search
			val otherTextFieldType = new FieldType()
			otherTextFieldType.setIndexed(false)
			otherTextFieldType.setTokenized(false)
			otherTextFieldType.setStored(true)

			// the tags, POS, NER etc.
			doc.add(new Field(TAGS, textFileToString(mkFileName(paperDir, paperPrefix, section, "tags")), otherTextFieldType))
			// syntactic information, both constituent based and dependency based
			doc.add(new Field(PARSE, textFileToString(mkFileName(paperDir, paperPrefix, section, "parse")), otherTextFieldType))
			//doc.add(new Field(SDEP, textFileToString(mkFileName(paperDir, paperPrefix, section, "sdep")), otherTextFieldType))
			doc.add(new Field(SDEPCC, textFileToString(mkFileName(paperDir, paperPrefix, section, "sdepcc")), otherTextFieldType))
			// info on protein complexes
			doc.add(new Field(NER, textFileToString(mkFileName(paperDir, paperPrefix, section, "ner")), otherTextFieldType))

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

	def runToolkit(paper:File) {
		val cmd = s"$toolkitDirectory/process_paper.sh ${paper.getAbsolutePath} ${paper.getParent}"
		logger.debug(s"Running command: $cmd")
		val exitCode = cmd.!
		logger.debug(s"Exit code = $exitCode")
		if(exitCode != 0) {
			// TODO: is this the best way to handle pipeline errors?
			throw new RuntimeException(s"ERROR: REACHToolkit failed on paper ${paper.getAbsolutePath}!")
		}
	}
}

object IndexerWithReachToolkit {
	val PAPERID = "paperid"
	val SECTION = "section"
	val TOK = "tok"
	val TAGS = "tags"
	val PARSE = "parse"
	val SDEPCC = "sdepcc"
	val NER = "ner"

	val EXT = "nxml"

	val VERSION = Version.LUCENE_42

	val logger = LoggerFactory.getLogger(classOf[IndexerWithReachToolkit])

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

		val indexer = new IndexerWithReachToolkit(toolkitDirectory, nCores)
		indexer.index(paperDirectory, indexDirectory, idFile)
	}
}
