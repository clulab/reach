package org.clulab.reach.darpa

import org.clulab.odin.Mention
import org.clulab.reach.mentions.{BioEventMention, BioMention}

object HyphenHandle {

  protected def swapOptionalEntries[A, B](map: Map[A, B], leftKey: A, rightKey: A): Map[A, B] = {
    // It may be that leftKey and rightKey aren't both populated.
    // When the keys are swapped, it is necessary to remove those that won't be replaced.

    def addOpt(map: Map[A, B], key: A, valueOpt: Option[B]): Map[A, B] =
        valueOpt.map { value => map + (key -> value) }.getOrElse(map)

    val leftValueOpt = map.get(leftKey)
    val rightValueOpt = map.get(rightKey)
    val mapWithNeither = map - leftKey - rightKey
    val mapWithOne = addOpt(mapWithNeither, leftKey, rightValueOpt)
    val mapWithBoth = addOpt(mapWithOne, rightKey, leftValueOpt)

    mapWithBoth
  }

  /**
    * Receives a sequence of BioMentions and flips the role of the arguments
    * when there's a "- ing" pattern
    * Example: The EM-inducing TFs (TWIST1, SNAIL1, SLUG, ZEB1, and FOXC2) in the CD45 - cells were determined using qRT-PCR.
    * A rule will match EM as controller and the other entities as controlled. This function will flip the roles
    * @param mentions The original mention sequence to operate on
    * @return A sequence of the same size with the modified BioMentions
    */
  def handleHyphens(mentions: Seq[BioMention]):Seq[BioMention] = {
    // For each mention we will ...
    mentions map {
      // ... consider it only if it's an event mention ...
      case event:BioEventMention => {
        // ... and if has a controller and the original text ...
        (event.controllerArgs(), event.document.text) match {
            // ... just make sure there's only a single controller ...
            case (Some(controllers), Some(ogText)) if controllers.size == 1 =>
              // Fetch the controller and the sentence object
              val controller = controllers.head
              val sentence = event.document.sentences(event.sentence)

              // Figure out it's offset on the original text of the source
              val start = controller.tokenInterval.last
              val end = if(sentence.words.length > start+1) start+1 else start

              // And fetch that chunk of text
              val text = ogText.slice(sentence.startOffsets(start), sentence.endOffsets(end))

              // If there is a hyphen and the next word after the hyphen finishes with "ing"
              if(text.contains('-') && text.toLowerCase.endsWith("ing")){
                // Flip the controller and controlled entities to create a new event
                val args = event.arguments
                val newArgs = swapOptionalEntries(args, "controller", "controlled")

                // Create a new event mention as a copy of the original but with that difference
                new BioEventMention(event.copy(arguments = newArgs))
              }
              // Otherwise don't touch it
              else
                event
            //... if there are more, better not to mess with it
            case _ => event
          }
      }
      //... otherwise don't touch it
      case m => m
    }
  }
}
