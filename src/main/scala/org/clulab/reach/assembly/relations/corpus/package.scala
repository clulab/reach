package org.clulab.reach.assembly.relations

import org.clulab.odin.Mention
import org.clulab.reach.assembly.sieves.SieveUtils
import org.clulab.reach.mentions.serialization.json.{CorefMentionOps, JSONSerializer}
import org.clulab.reach.mentions._
import com.typesafe.scalalogging.LazyLogging
import java.io.File


package object corpus extends LazyLogging {

  /** Additional attributes and methods for a [[CorefMention]] */
  implicit class EventOps(mention: CorefMention) extends CorefMentionOps(mention) {
    val eventLabel: String = mention.label
    val sentenceText: String = mention.sentenceObj.getSentenceText()
    // NOTE: if mention is a TB, trigger will simply be the mention (ex. BioProcess)
    val trigger = SieveUtils.findTrigger(mention)
  }

  /** Retrieves PubMed ID from Document.id of an Odin Mention */
  def getPMID(mention: Mention): String = getPMID(mention.document.id.get)
  /** Retrieves PubMed ID from Document.id of an Odin Mention */
  def getPMID(docid: String): String = s"PMC${docid.split("_")(0).replace("PMC", "")}"

  /** Create a dataset from a directory of json files, where each file represents the reach reading results for that paper */
  def datasetLUT(jsonDir: String): Map[String, Vector[CorefMention]] = {
    def parseJSON(f: File): Option[Seq[CorefMention]] = try {
      Some(JSONSerializer.toCorefMentions(f))
    } catch {
      case e: org.json4s.ParserUtil.ParseException => {
        logger.info(s"Failed to parse $f")
        None
      }
    }
    val docMentionPairs = for {
      f <- new File(jsonDir).listFiles
      cms: Option[Seq[CorefMention]] = parseJSON(f)
      if cms.nonEmpty
      paperID = getPMID(cms.get.head)
    } yield  paperID -> cms.get.toVector

    docMentionPairs.toMap.withDefaultValue(Vector.empty[CorefMention])
  }
}
