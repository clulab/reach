package edu.arizona.sista.odin.extern.export

import java.io.File
import java.util.Date

import edu.arizona.sista.odin.Mention

/**
  * Trait for output formatters which output JSON formats.
  *   Written by Tom Hicks. 5/22/2015.
  *   Last Modified: Initial creation.
  */
trait JsonOutputter {

  /**
    * Output the given mentions to the given output file in some JSON-based format.
    * Default method to be overridden by each JSON output formatter.
    */
  def toJSON (allMentions:Seq[Mention], outFile:File) = {
  }

  /**
    * Output the given mentions to the given output file in some JSON-based format.
    * The processing start and stop date/times are given in case they are needed.
    * Default method to be overridden by each JSON output formatter.
    */
  def toJSON (allMentions:Seq[Mention], startTime:Date, endTime:Date, outFile:File) = {
  }

}
