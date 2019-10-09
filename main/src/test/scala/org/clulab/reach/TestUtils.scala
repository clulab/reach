package org.clulab.reach

import java.io.File

import ai.lum.nxmlreader.{NxmlDocument, NxmlReader}
import org.clulab.odin._
import org.clulab.processors.Document
import org.clulab.reach.mentions._
import org.clulab.reach.utils.MentionManager

import scala.io.Source
import scala.util.Try


/**
  * Utility methods for the tests in this directory
  * Last Modified: Update for processing annotators.
  */
object TestUtils {

  // get a new or current instance of a processor annotator
  val procAnnotator = ProcessorAnnotatorFactory()

  // Inner object that contains the annotations to test context
  object Context{
    def nxml1 = Source.fromURL(getClass.getResource("/inputs/nxml/PMC2597732.nxml")).mkString
    def nxml2 = Source.fromURL(getClass.getResource("/inputs/nxml/PMC3189917.nxml")).mkString
    def nxml3 = Source.fromURL(getClass.getResource("/inputs/nxml/PMC1289294.nxml")).mkString

    case class Annotation(friesEntries:Seq[FriesEntry], documents:Seq[Document], entitiesPerEntry:Seq[Seq[BioMention]], mentions:Seq[BioMention])

    def annotatePaper(nxml:String):Annotation = {
      val friesEntries = mkEntries(testReader.parse(nxml))
      val documents = friesEntries map (e => testReach.mkDoc(e.text, e.name, e.chunkId))
      val entitiesPerEntry =  for (doc <- documents) yield testReach.extractEntitiesFrom(doc)
      val mentions = testReach.extractFrom(friesEntries, documents)

      Annotation(friesEntries, documents, entitiesPerEntry, mentions)
    }

    def mkEntries(nxmldoc: NxmlDocument): Seq[FriesEntry] = Seq(new FriesEntry(nxmldoc))

    val paperAnnotations = Map(1 -> annotatePaper(nxml1)/*, 2 -> annotatePaper(nxml2), 3 -> annotatePaper(nxml3)*/)
  }

  /**
    * Read contents of rule file in the classpath, given some path
    * @param path the path to a file
    * @return file contents as a String
    */
  def readFileContent(path: String) = {
    val stream = getClass.getClassLoader.getResourceAsStream(path)
    val source = Source.fromInputStream(stream)
    val data = source.mkString
    source.close()
    data
  }

  def readResourceAsFile(path: String): File = {
    val url = getClass.getClassLoader.getResource(path)
    new File(url.toURI)
  }

  val testReach = PaperReader.reachSystem // All tests should use this system!
  val testReader = new NxmlReader

  val docId = "testdoc"
  val chunkId = "1"
  val mentionManager = new MentionManager()

  def getMentionsFromText(text: String): Seq[Mention] = PaperReader.getMentionsFromText(text)

  def getBioMentionsFromText(text: String): Seq[BioMention] = for {
    m <- getMentionsFromText(text)
  } yield m.toBioMention

  def getCorefmentionsFromText(text: String): Seq[CorefMention] = for {
    m <- getMentionsFromText(text)
  } yield m.toCorefMention

  def getBioMentions(text:String, verbose:Boolean = false):Seq[BioMention] = {
    val entry = FriesEntry(docId, chunkId, "example", "example", isTitle = false, text)
    val result = testReach.extractFrom(entry)
    //if(! result.isSuccess)
      //throw new RuntimeException("ERROR: getBioMentions failed on sentence: " + text)
    //val mentions = printMentions(result, verbose)
    //mentions
    result
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

    println("=====================")
    val classifierTemp = new org.clulab.polarity.ml.DeepLearningPolarityClassifier()
    println(mentions(0).sentenceObj.words)
    val bioEventM = mentions filter {
      case em:BioEventMention => true
      case _ => false
    }
    for (bioM <- bioEventM){
      if (bioM.isInstanceOf[BioEventMention]){
        val masked_lemmas = classifierTemp.maskEvent(bioM.sentenceObj.words.clone(), bioM.asInstanceOf[BioEventMention] , "tag")
        println("\t",masked_lemmas.toSeq)

        if (bioM.arguments.contains("controller")){
          println("\t\tcontroller: ",bioM.arguments("controller").head.text, "\ttype:", bioM.arguments("controller").head.getClass.getName, bioM.arguments("controller").head.start, bioM.arguments("controller").head.end)
        }else {println("\t\tNo controller")}
        if (bioM.arguments.contains("controlled")){
          println("\t\tcontrolled: ",bioM.arguments("controlled").head.text, "\ttype:", bioM.arguments("controlled").head.getClass.getName, bioM.arguments("controlled").head.start, bioM.arguments("controlled").head.end)
        }else {println("\t\tNo controlled")}
      }
    }
    scala.io.StdIn.readLine()


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

  def displayMentions(mentions: Seq[Mention], doc: Document): Unit = display.displayMentions(mentions, doc)

  implicit class MentionTestUtils(mention: Mention) {

    def modifications: Set[Modification] = mention.toBioMention.modifications

    // FIXME: this is nasty.  How can I pattern match on something that extends a trait?
    def ptms: Set[PTM] = modifications.map {
      case ptm if ptm.isInstanceOf[PTM] => ptm.asInstanceOf[PTM]
    }
  }

  implicit class BioMentionTestUtils(mention: BioMention) {

    def hasMutation(mutant: String, subType: String): Boolean = mention match {
      case empty if mention.modifications.isEmpty => false
      case _ =>
        val mutations =
          mention.modifications
            .filter(_.isInstanceOf[Mutant])
            .map(_.asInstanceOf[Mutant])
        mutations.exists(mutation => mutation.evidence.text.contains(mutant) && mutation.matches(subType))
    }

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
