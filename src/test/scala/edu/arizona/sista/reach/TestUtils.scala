package edu.arizona.sista.reach

import edu.arizona.sista.reach.nxml.FriesEntry
import edu.arizona.sista.reach.display._
import edu.arizona.sista.reach.extern.export.MentionManager
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.odin._
import edu.arizona.sista.processors.Document
import scala.util.Try

/**
 * Utility methods for the tests in this directory
 */
object TestUtils {
  val testReach = new ReachSystem // All tests should use this system!
  val docId = "testdoc"
  val chunkId = "1"
  val mentionManager = new MentionManager()

  def getBioMentions(text:String, verbose:Boolean = false):Seq[BioMention] = {
    val entry = FriesEntry(docId, chunkId, "example", "example", isTitle = false, text)
    val result = Try(testReach.extractFrom(entry))
    if(! result.isSuccess)
      throw new RuntimeException("ERROR: getBioMentions failed on sentence: " + text)
    val mentions = printMentions(result, verbose)
    mentions
  }

  def getEntities(sentence: String, verbose:Boolean = false):Seq[BioMention] = {
    val doc = testReach.mkDoc(sentence, docId, chunkId)
    // Get only entities (after modficationEngine)
    val result = Try(testReach.extractEntitiesFrom(doc))
    if(! result.isSuccess)
      throw new RuntimeException("ERROR: getBioMentions failed on sentence: " + sentence)
    val mentions = printMentions(result, verbose)
    mentions
  }

  def getEventSites(m:BioMention) = m.toBioMention.modifications.filter(_.isInstanceOf[EventSite])

  def printMentions(result:Try[Seq[BioMention]], verbose:Boolean = false):Seq[BioMention] = {
    val mentions = result.get
    if(verbose) {
      println("Mentions:")
      for (m <- mentions) {
        mentionManager.mentionToStrings(m).foreach(println(_))
        println()
      }
    }
    mentions
  }

  def summarizeError(sentence: String, label: String, assignedParty: String): String =
    s"Failed ${label} test for sentence:\n\t$sentence\n\tResponsible: ${assignedParty}"

  def hasEventWithArguments(label: String, args: Seq[String], mentions: Seq[Mention]): Boolean = {
    for (m <- mentions) {
      if (!m.isInstanceOf[TextBoundMention]) {
        if (m.labels contains label) {
          // found the label

          // This is only necessary because we decided to make complexes using relation mentions.
          // ex. GTP hydrolysis for Ras => "Ras-GTP" becomes the label of a resultant relation mention.
          // We really shouldn't be doing this sort of thing in a mention.
          val allText = s"${
            m.arguments.values.
              flatten
              .map(arg => arg.toCorefMention.antecedent.getOrElse(arg).asInstanceOf[CorefMention].text)
              .mkString(" ")
          }".toLowerCase

          if (args.forall { arg => allText contains arg.toLowerCase}) {
            //println(s"\t==> found event mention: ${m.text}")
            return true
          }
        }
      }
    }
    false
  }

  def hasEntity(text: String, mentions: Seq[Mention]): Boolean = {
    for (m <- mentions) {
      if (m.isInstanceOf[TextBoundMention]) {
        val tm = m.toCorefMention.antecedent.getOrElse(m).asInstanceOf[CorefMention]
        if (tm.text == text) {
          //println(s"\t==> found entity mention: ${tm.text}")
          return true
        }
      }
    }
    false
  }

  def hasEntityWithSite(text: String, site: String, mentions: Seq[Mention]): Boolean = {
    for (m <- mentions) {
      if (m.isInstanceOf[RelationMention]) {
        val rm = m.asInstanceOf[RelationMention]
        if (rm.arguments.contains("site") &&
          contains(rm.arguments.get("site").get.map(s => s.toCorefMention.antecedent.getOrElse(s).asInstanceOf[CorefMention]), site) &&
          rm.arguments.contains("protein") &&
          contains(rm.arguments.get("protein").get.map(p => p.toCorefMention.antecedent.getOrElse(p).asInstanceOf[CorefMention]), text)) {
          //println(s"\t==> found entity mention with site: ${rm.text}")
          return true
        }
      }
    }
    false
  }

  def contains(mentions: Seq[Mention], text: String): Boolean = {
    for (m <- mentions) if (m.toCorefMention.antecedent.getOrElse(m).asInstanceOf[CorefMention].text == text) return true
    false
  }

  def hasPositiveRegulationByEntity(controllerEntity: String, controlledLabel: String, controlledArgs: Seq[String], mentions: Seq[Mention]): Boolean =
    hasRegulationByEntity("Positive_regulation", controllerEntity, controlledLabel, controlledArgs, mentions)

  def hasNegativeRegulationByEntity(controllerEntity: String, controlledLabel: String, controlledArgs: Seq[String], mentions: Seq[Mention]): Boolean =
    hasRegulationByEntity("Negative_regulation", controllerEntity, controlledLabel, controlledArgs, mentions)

  def hasRegulationByEntity(label: String,
                            controllerEntity: String,
                            controlledLabel: String,
                            controlledArgs: Seq[String],
                            mentions: Seq[Mention]): Boolean = {
    for (m <- mentions) {
      if (!m.isInstanceOf[TextBoundMention]) {
        if (m.labels contains label) {
          // found the regulation label
          val controller = m.arguments.get("controller")
          val controlled = m.arguments.get("controlled")

          if (controller.isDefined && controlled.isDefined && controlled.get.head.isInstanceOf[EventMention]) {
            // some obvious sanity checks
            val controlledEventArg = controlled.get.head
            val controlledEvent = controlledEventArg.toCorefMention.antecedent.getOrElse(controlledEventArg).asInstanceOf[CorefMention]
            if (controller.get.head.text == controllerEntity && // found the controller entity
              controlledEvent.label == controlledLabel) {
              val allText = s"${m.text} ${controlledEvent.arguments.values
                .flatten
                .map(a => a.toCorefMention.antecedent.getOrElse(a).asInstanceOf[CorefMention])
                .map(_.text)
                .mkString(" ")}".toLowerCase

              if (controlledArgs.forall{arg => allText contains arg.toLowerCase}) {
                //println(s"\t==> found event mention: ${m.text}")
                return true
              }
            }
          }
        }
      }
    }
    false
  }

  def hasPositiveActivation(controllerEntity: String, controlledEntity: String, mentions: Seq[Mention]): Boolean =
    hasActivation("Positive_activation", controllerEntity, controlledEntity, mentions)

  def hasNegativeActivation(controllerEntity: String, controlledEntity: String, mentions: Seq[Mention]): Boolean =
    hasActivation("Negative_activation", controllerEntity, controlledEntity, mentions)

  def hasActivation(label: String,
                    controllerEntity: String,
                    controlledEntity: String,
                    mentions: Seq[Mention]): Boolean = {
    for (m <- mentions) {
      if (!m.isInstanceOf[TextBoundMention]) {
        if (m.labels contains label) {
          // found the label
          val controller = m.arguments.get("controller")
          val controlled = m.arguments.get("controlled")

          if (controller.isDefined && controlled.isDefined &&
              controlled.get.head.isInstanceOf[TextBoundMention] &&
              controller.get.head.isInstanceOf[TextBoundMention]) {

            val controllerText = controller.get.head.toCorefMention.antecedent.getOrElse(controller.get.head).asInstanceOf[CorefMention].text
            val controlledText = controlled.get.head.toCorefMention.antecedent.getOrElse(controlled.get.head).asInstanceOf[CorefMention].text

            // some obvious sanity checks
            if (controllerText == controllerEntity && // found the controller entity
                controlledText == controlledEntity) {
              return true
            }
          }
        }
      }
    }
    false
  }

  def header(name: String) {
    println(s"\n${":" * 20}$name${":" * 20}\n")
  }

  def displayMentions(mentions: Seq[Mention], doc: Document): Unit = {
    val mentionsBySentence = mentions groupBy (_.sentence) mapValues (_.sortBy(_.start)) withDefaultValue Nil
    for ((s, i) <- doc.sentences.zipWithIndex) {
      println(s"sentence #$i")
      println(s.getSentenceText)
      println
      mentionsBySentence(i).sortBy(_.label) foreach displayMention
      println("=" * 50)
    }
  }

  implicit class MentionTestUtils(mention: BioMention) {

    def hasMutation(mutant: String): Boolean = mention match {
      case empty if mention.modifications.isEmpty => false
      case _ =>
        val mutations =
          mention.modifications
            .filter(_.isInstanceOf[Mutant])
            .map(_.asInstanceOf[Mutant])
        mutations.exists(_.evidence.text contains mutant)
    }

    def hasMutation: Boolean = mention.modifications.exists(_.isInstanceOf[Mutant])

    def countMutations: Int = mention.modifications.count(_.isInstanceOf[Mutant])
  }
}
