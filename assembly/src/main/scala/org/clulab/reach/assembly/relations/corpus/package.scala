package org.clulab.reach.assembly.relations

import org.clulab.odin.Mention
import org.clulab.reach.assembly.sieves.SieveUtils
import org.clulab.reach.serialization.json.{CorefMentionOps, JSONSerializer}
import org.clulab.reach.mentions._
import com.typesafe.scalalogging.LazyLogging
import scala.collection.GenSeq
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
  def datasetLUT(jsonDir: File): Map[String, Vector[CorefMention]] = datasetLUT(jsonDir.listFiles)
  def datasetLUT(jsonFiles: GenSeq[File]): Map[String, Vector[CorefMention]] = {
    val docMentionPairs = jsonFiles.filter(_.getName.endsWith(".json")).map{ f: File =>
      logger.debug(s"parsing ${f.getName}")
      val cms: Vector[CorefMention] = JSONSerializer.toCorefMentions(f).toVector
      if (cms.nonEmpty) logger.debug(s"successfully parsed ${f.getName}")
      val paperID = getPMID(cms.head)
      paperID -> cms
    }
    docMentionPairs.seq.toMap.withDefaultValue(Vector.empty[CorefMention])
  }
}
