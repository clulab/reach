package edu.arizona.sista.bionlp.reach.ruler

import edu.arizona.sista.bionlp.reach.core.RelationMention
import edu.arizona.sista.matcher.{TextBoundMention, EventMention, Mention}

/**
 * Utility methods for the tests in this directory
 * User: mihais
 * Date: 1/5/15
 */
object DarpaEvalUtils {
  def hasEventWithArguments(label:String, args:Seq[String], mentions:Seq[Mention]):Boolean = {
    for(m <- mentions) {
      if(m.isInstanceOf[EventMention]) {
        val em = m.asInstanceOf[EventMention]
        if(em.label == label) { // found the label
        var count = 0
          for(arg <- args) {
            for (a <- em.arguments.values.flatten) {
              if(arg == a.text) {
                count += 1
              }
            }
          }
          if(count == args.size) {
            // found all args as well

            println(s"\t==> found event mention: ${em.text}")
            return true
          }
        }
      }
    }
    false
  }

  def hasEntity(text:String, mentions:Seq[Mention]):Boolean = {
    for(m <- mentions) {
      if (m.isInstanceOf[TextBoundMention]) {
        val tm = m.asInstanceOf[TextBoundMention]
        if (tm.text == text) {
          println(s"\t==> found entity mention: ${tm.text}")
          return true
        }
      }
    }
    false
  }

  def hasEntityWithSite(text:String, site:String, mentions:Seq[Mention]):Boolean = {
    for(m <- mentions) {
      if (m.isInstanceOf[RelationMention]) {
        val rm = m.asInstanceOf[RelationMention]
        if (rm.arguments.contains("Site") &&
          contains(rm.arguments.get("Site").get, site) &&
          rm.arguments.contains("Protein") &&
          contains(rm.arguments.get("Protein").get, text)) {
          println(s"\t==> found entity mention with site: ${rm.text}")
          return true
        }
      }
    }
    false
  }

  def contains(mentions:Seq[Mention], text:String):Boolean = {
    for(m <- mentions) if(m.text == text) return true
    false
  }

}
