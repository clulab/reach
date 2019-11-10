package org.clulab.polarity.ml

import org.clulab.reach.mentions.BioEventMention

package object data {

  /**
    * Any val class to marshal a bio event mention to the lemma sequence used as input to the RNN
    * @param event to be operated on
    */
  implicit class RRNInput(val event:BioEventMention) extends AnyVal {

    /**
      * Returns the lemmas passed to a LSTM classifier for polarity
      * @param window in number of tokens to the left and right of the event text
      * @return The lemmas comprising the text event plus the windows to the left and right
      */
    def lemmasForPolarity(window: Int = 0): Seq[String] = {
      val lemmas = event.sentenceObj.lemmas.getOrElse(throw new UnsupportedOperationException("Lemmas not annotated in for the event mention"))

      val length = event.sentenceObj.size
      val start = Seq(0, event.start - window).max
      val end = Seq(length - 1, event.end + window).min

      lemmas.slice(start, end)
    }
  }

}
