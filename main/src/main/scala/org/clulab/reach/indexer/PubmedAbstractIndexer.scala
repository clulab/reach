package org.clulab.reach.indexer

import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document._
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.FSDirectory
import org.clulab.reach.indexer.NxmlIndexer.logger
import org.clulab.reach.utils.Preprocess
import org.clulab.utils.{FileUtils, StringUtils}

import java.nio.file.Paths
import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.xml.XML

case class JournalMetadata(name:String,
                           issn:String,
                           volume:Int,
                           issue:Int,
                           published:String)

case class PubmedAbstract(pmid:String,
                          title:String,
                          text:String,
                          doi:Option[String],
                          journal:Option[JournalMetadata])

class PubmedAbstractIndexer {

  def index(docsDir:String, indexDir:String): Unit = {

    val files = FileUtils.walkTree(docsDir).filter(f => f.getName.endsWith(".xml")).toList // Since Dec 2021, pubmed files use xml extensions
    logger.info(s"Preparing to index ${files.length} files...")
    val parser = PubmedReader()


    // index
    val analyzer = new StandardAnalyzer
    val config = new IndexWriterConfig(analyzer)
    val index = FSDirectory.open(Paths.get(indexDir))
    val writer = new IndexWriter(index, config)


    var count = 0

    for (file <- files) {
      // Preprocess bio text
      val source = Source.fromFile(file)
      val rawText = source.getLines.mkString("\n")
      source.close()

      var totalIndexed = 0

      Try(parser.parse(rawText)) match {
        case Success(abstracts) =>
          logger.info(s"${abstracts.size} contained in file")
          val indexed = addDocs(writer, abstracts)
          totalIndexed += indexed
        case Failure(e) =>
          logger.error(s"WARNING: NxmlReader failed on file $file")
      }

      logger.info(s"Indexed $totalIndexed abstracts")

      count += 1
      if(count % 100 == 0)
        logger.info(s"Indexed $count/${files.size} files.")

    }
    writer.close()
    logger.info(s"Indexing complete. Indexed $count/${files.size} files.")
  }

  def addDocs(writer:IndexWriter, abstracts:Iterable[PubmedAbstract]): Int = {
    abstracts map (abs => Try(addDoc(writer, abs))) count { case Success(_) => true ; case Failure(_) => false }
  }

  def addDoc(writer:IndexWriter, abs:PubmedAbstract): Unit = {
    val d = new Document
    d.add(new TextField("text", abs.text, Field.Store.YES))
    d.add(new TextField("title", abs.title, Field.Store.YES))
    d.add(new StringField("pmid", abs.pmid, Field.Store.YES))

    for(doi <- abs.doi)
        d.add(new StringField("doi", doi, Field.Store.YES))

    for(meta <-  abs.journal) {
        d.add(new StringField("published", meta.published, Field.Store.YES))
        d.add(new TextField("journal", meta.name, Field.Store.YES))
        d.add(new StringField("issn", meta.issn, Field.Store.YES))
        d.add(new IntPoint("volume", meta.volume))
        d.add(new StoredField("volume", meta.volume))
        d.add(new IntPoint("issue", meta.issue))
        d.add(new StoredField("issue", meta.issue))
    }

    writer.addDocument(d)
  }

}

object PubmedAbstractIndexer extends App {

  val props = StringUtils.argsToProperties(args)
  val indexDir = props.getProperty("index")
  val docsDir = props.getProperty("docs")

  val indexer = new PubmedAbstractIndexer
  indexer.index(docsDir, indexDir)

}

case class PubmedReader() {
  val preproc = new Preprocess
  def parse(contents:String):Iterable[PubmedAbstract] = {
    val xml = XML.loadString(contents)
    for(article <- xml \ "PubmedArticle") yield {
      val pmid = (article \ "MedlineCitation" \ "PMID").text
      val title = (article \ "MedlineCitation" \ "Article" \ "ArticleTitle").text
      val text = (article \ "MedlineCitation" \ "Article" \ "Abstract" \ "AbstractText").text
      val articleIds =
        for {
          id <- article \ "PubmedData" \ "ArticleIdList" \ "ArticleId"
          if (id \ "@IdType").text == "doi"
        } yield id.text

      val doi = articleIds match {
        case id :: _ => Some(id)
        case Nil => None
      }

      val metadata = (article \ "MedlineCitation" \ "Article" \ "Journal").toList match {
        case mt::__ =>
          val issn = (mt \ "ISSN").text
          val name = (mt \ "Title").text
          val volume =  Try((mt \ "JournalIssue" \ "Volume").text.toInt) match {
            case Success(v) => v
            case Failure(_) => -1
          }
          val issue = Try((mt \ "JournalIssue" \ "Issue").text.toInt)  match {
            case Success(v) => v
            case Failure(_) => -1
          }

          val pubdate = mt \ "JournalIssue" \ "PubDate"

          val year = (pubdate \ "Year").text
          val month = (pubdate \ "Month").text
          val day = (pubdate \ "Day").text
          val published = s"$year-$month-$day".toLowerCase

          Some(JournalMetadata(name, issn, volume, issue, published))

        case Nil => None
      }

      PubmedAbstract(pmid, preproc.preprocessText(title), preproc.preprocessText(text), doi, metadata)
    }
  }
}
