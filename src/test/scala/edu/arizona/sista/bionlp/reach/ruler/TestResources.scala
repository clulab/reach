package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.processors.bionlp.BioNLPProcessor

/**
 * Created by gus on 1/15/15.
 */
class TestResources {

}

object TestResources {
  val bioproc = {
    val proc = new BioNLPProcessor
    proc.annotate("poop") // To trick BANNER into not failing...
    proc
  }

}
