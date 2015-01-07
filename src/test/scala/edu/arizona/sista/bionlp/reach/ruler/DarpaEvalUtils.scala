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

  def hasUpRegulationByEntity(controllerEntity:String, controlledLabel:String, controlledArgs:Seq[String], mentions:Seq[Mention]):Boolean =
    hasRegulationByEntity("UpRegulation", controllerEntity, controlledLabel, controlledArgs, mentions)

  def hasDownRegulationByEntity(controllerEntity:String, controlledLabel:String, controlledArgs:Seq[String], mentions:Seq[Mention]):Boolean =
    hasRegulationByEntity("DownRegulation", controllerEntity, controlledLabel, controlledArgs, mentions)

  def hasRegulationByEntity(label:String,
                            controllerEntity:String,
                            controlledLabel:String,
                            controlledArgs:Seq[String],
                            mentions:Seq[Mention]):Boolean = {
    for(m <- mentions) {
      if(m.isInstanceOf[EventMention]) {
        val em = m.asInstanceOf[EventMention]
        if(em.label == label) { // found the regulation label
          val controller = em.arguments.get("Controller")
          val controlled = em.arguments.get("Controlled")

          if(controller.isDefined && controlled.isDefined && controlled.get.head.isInstanceOf[EventMention]) { // some obvious sanity checks
            val controlledEvent = controlled.get.head.asInstanceOf[EventMention]
            if(controller.get.head.text == controllerEntity && // found the controller entity
               controlledEvent.label == controlledLabel) { // found the correct label for the controlled event
              var count = 0
              for(arg <- controlledArgs) {
                for (a <- controlledEvent.arguments.values.flatten) {
                  if(arg == a.text) {
                    count += 1
                  }
                }
              }
              if(count == controlledArgs.size) {
                // found all args for the controlled event as well
                println(s"\t==> found ${label} with Controller:${controllerEntity} and Controlled:${controlledLabel} with arguments:${controlledArgs.mkString(",")}")
                return true
              }
            }
          }
        }
      }
    }
    false
  }

  def header(name:String) {
    println(name + " ==>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  }
}
