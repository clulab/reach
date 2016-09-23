package org.clulab.reach.context

import util.{Try, Success, Failure}
import org.clulab.reach.mentions.BioMention

object ContextClass extends Enumeration{
  val Species, Organ, CellLine, CellType, Cellular_component, TissueType, Undetermined = Value

  def getContextClass(mention:BioMention):Value = {
    // Try to parse all the labels
    val tryClasses:Seq[Try[Value]] = mention.labels map (l => Try(ContextClass.withName(l)))

    // Collect the matching labels
    val classes:Seq[Value] = tryClasses collect { case Success(c) => c }

    // It should be of a single type (for now)
    if(classes.size == 0)
      Undetermined
    else{
      assert(classes.size == 1, "A mention has more than one context class label")
      classes(0)
    }
  }

  def isContextMention(mention:BioMention):Boolean = getContextClass(mention) != Undetermined

}
