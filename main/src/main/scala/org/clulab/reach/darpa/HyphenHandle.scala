package org.clulab.reach.darpa

import org.clulab.reach.mentions.{BioEventMention, BioMention}

object HyphenHandle {
  /**
    * Receives a sequence of BioMentions and flips the role of the arguments
    * when there's a "- ing" pattern
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

              // Figrue out it's offset on the original text of the source
              val start = controller.tokenInterval.last
              val end = if(sentence.words.length > start+1) start+1 else start

              // And fetch that chunk of text
              val text = ogText.slice(sentence.startOffsets(start), sentence.endOffsets(end))

              // If there is a hyphen and the next word after the hyphen finishes with "ing"
              if(text.contains('-') && text.toLowerCase.endsWith("ing")){
                // Flip the controller and controlled entities to create a new event
                val args = event.arguments
                val newArgs = args + ("controller" -> args("controlled")) + ("controlled" -> args("controller"))

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
