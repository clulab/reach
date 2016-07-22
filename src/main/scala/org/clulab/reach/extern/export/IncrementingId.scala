package org.clulab.reach.extern.export

/**
 * Implements an incrementing identification string for numbering entities.
 * User: mihais
 * Date: 8/28/15
 */
class IncrementingId(start:Int = 0) {
  protected var cntr = start

  /** Return the current identification string. */
  def currentId (): String = { s"$cntr" }

  /** Increment counter and return new identification string. */
  def genNextId (): String = {
    cntr = cntr + 1
    currentId()
  }

  /** Increment counter and return new identification string. */
  def genNextIdWithFormat (formatString:String): String = {
    cntr = cntr + 1
    formatString.format(cntr)
  }

}
