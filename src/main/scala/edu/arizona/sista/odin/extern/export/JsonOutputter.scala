package edu.arizona.sista.odin.extern.export

import java.io.File
import java.util.Date

import edu.arizona.sista.bionlp.FriesEntry
import edu.arizona.sista.odin.Mention

/**
  * Trait for output formatters which output JSON formats.
  *   Written by Tom Hicks. 5/22/2015.
  *   Last Modified: Rename file writing method. Add method to return one big string.
  */
trait JsonOutputter {

  /**
    * Returns the given mentions in some JSON-based format, as one big string.
    * The processing start and stop date/times are given.
    * The input filename prefix is provided for use by the generator routines, as needed.
    * Default method to be overridden by each JSON output formatter.
    */
  def toJSON (paperId:String,
              allMentions:Seq[Mention],
              paperPassages:Seq[FriesEntry],
              startTime:Date,
              endTime:Date,
              outFilePrefix:String): String

  /**
    * Outputs the given mentions to the given output file in some JSON-based format.
    * The processing start and stop date/times are given.
    * The output file is given as a prefix, in case outputters choose to generate
    * multiple output files (see HansOutput)
    * Default method to be overridden by each JSON output formatter.
    */
  def writeJSON (paperId:String,
                 allMentions:Seq[Mention],
                 paperPassages:Seq[FriesEntry],
                 startTime:Date,
                 endTime:Date,
                 outFilePrefix:String)

}
