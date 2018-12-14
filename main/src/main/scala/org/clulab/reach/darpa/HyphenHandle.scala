package org.clulab.reach.darpa

import org.clulab.odin.{Mention, SynPath, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.reach.mentions.{BioEventMention, BioMention}

object HyphenHandle {
  def handleHyphens(mentions: Seq[BioMention]):Seq[BioMention] = {
    mentions map {
      case event:BioEventMention => {
        (event.controllerArgs(), event.document.text) match {
            case (Some(controllers), Some(ogText)) if controllers.size == 1 =>
              val controller = controllers.head
              val sentence = event.document.sentences(event.sentence)

              val start = controller.tokenInterval.last
              val end = if(sentence.words.length > start+1) start+1 else start

              val text = ogText.slice(sentence.startOffsets(start), sentence.endOffsets(end))

              if(text.contains('-')){
                val args = event.arguments
                // Flip the controller and controlled entities to create a new event
                val newArgs = args + ("controller" -> args("controlled")) + ("controlled" -> args("controller"))

                new BioEventMention(event.copy(arguments = newArgs))
              }
              else
                event
            case _ => event
          }
      }
      case m => m
    }
  }
}
