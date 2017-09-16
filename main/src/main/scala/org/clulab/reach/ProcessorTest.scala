package org.clulab.reach

import org.clulab.processors.clu.CluProcessor

/**
  * A simple test to make sure we can load the processors we need
  * User: mihais
  * Date: 9/16/17
  */
object ProcessorTest {
  def main(args: Array[String]): Unit = {
    val proc = new CluProcessor()
    proc.annotate("This is a test.")
  }
}
