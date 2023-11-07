import org.clulab.processors.Sentence
import org.clulab.reach.PaperReader
import org.clulab.reach.mentions.{BioEventMention, BioMention, BioRelationMention, BioTextBoundMention}

object UsageDemo extends App {

  def sentenceObjectDetails(sentence:Sentence, senIx:Int): Unit = {
      println(s"Details of sentence ${senIx + 1}:")
      print("Sentence text:\t")
      // Reconstruct the sentence easily with this
      println(sentence.getSentenceText)
      println()
      println("Word\tLemma\tPOS\tEntity")
      // Fetch all the token-level information of the current sentence
      val (words, lemmas, tags, entities) = (sentence.words, sentence.lemmas.get, sentence.tags.get, sentence.entities.get)
      for (i <- words.indices) {
        println(s"${words(i)}\t${lemmas(i)}\t${tags(i)}\t${entities(i)}")
      }
      println()
  }

  def mentionDetails(m:BioMention): Unit = {
    // Print the text that spans the whole mentions
    println(s"Mention text: ${m.text}")
    // Main label of the mention, which represents the "type" of the extraction
    println(s"Label: ${m.label}")
    // The mention will have more than one label. These will be all the entries on the branch of the taxonomy, from the root to above's label
    println(s"Labels: ${m.labels.mkString(", ")}")
    // There are three main type of mentions: TextBound, Event and Relation, for text bound mentions, that is it, for the others, we have the following
    m match {
      case _: BioTextBoundMention =>
        println("Event Type: Text bound")
      case e: BioEventMention =>
        println("Mention type: Event")
        // Events have a trigger, which is the keyword/phrase that fires the extraction. The trigger is a text bound mention itself
        println(s"Trigger: ${e.trigger.text}")
        // Events have arguments. Each argument has a "name" and one or more values. Values are text bound mentions themselves
        println("Arguments:")
        for ((name, values) <- e.arguments) {
          println(s"Name: $name\tValue: ${values.map(_.text).mkString(", ")}")
        }
      case r: BioRelationMention =>
        println("Mention type: Relation")
        // Relations don't have a trigger. That is the main difference between events and relations in this system
        // Relations have arguments. Each argument has a "name" and one or more values. Values are text bound mentions themselves
        println("Arguments:")
        for ((name, values) <- r.arguments) {
          println(s"Name: $name\tValues: ${values.map(_.text).mkString(", ")}")
        }
    }
    println()
  }


  // Generate extractions from a string
  val reachSystem = PaperReader.reachSystem
  val doc = reachSystem.mkDoc(text="RAS phosphorylates MEK. IL-6 regulates inflammation", docId = "Sample Text") // Create a document object, which packs all the NLP annotations
  val stringMentions = reachSystem.extractFrom(doc) // Run the IE system, which applies the rules

  /* Example of how to interact with the document object */

  // How to get the sentences from a document object
  doc.sentences.zipWithIndex foreach {
    case (s, i) => sentenceObjectDetails(s, i) // See the definition of this function to understand how to access the NLP annotations in each sentence
  }

  /* Example of how to interact with the extractions (mentions) detected using Odin rules */
  stringMentions foreach mentionDetails // See the definition of this function to understand how to access mentions' fields and their meaning

  // Generate extractions (mentions) from nxml file
  val entry = PaperReader.getEntryFromPaper("paper.nxml")
  val nxmlMentions = PaperReader.getMentionsFromEntry(entry).map(_.asInstanceOf[BioMention])

  println("XML paper mention details")
  println()
  nxmlMentions foreach mentionDetails
}
