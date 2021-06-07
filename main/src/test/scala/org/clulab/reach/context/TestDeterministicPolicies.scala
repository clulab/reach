package org.clulab.reach.context

import org.scalatest.{Matchers, FlatSpec}

import org.clulab.reach.mentions._
import org.clulab.reach.ReachSystem
import org.clulab.reach.TestUtils._
import org.clulab.reach.TestUtils.Context._

class TestDeterministicPolicies extends FlatSpec with Matchers {

  def contextAssignmentBehavior(paperNum:Int){
    info("Testing context assignment")

    val x = paperAnnotations(paperNum).mentions

    val tbMentions:Seq[BioTextBoundMention] = x.filter{
      case tm:BioTextBoundMention => true
      case _ => false
    }.map(_.asInstanceOf[BioTextBoundMention])


    val mentions:Seq[BioEventMention] = x.filter{
        case em:BioEventMention => true
        case _ => false
      }.map(_.asInstanceOf[BioEventMention])

    val context = mentions.map{
        _.context.getOrElse(Map[String,Seq[String]]())
    }

    it should "Have some context" in {
      // Remove/Change the filter if context is attached to other BioMentions

      val size = context.flatMap(_.values.flatten).size


      info(s"The number of context resolutions is: $size")
      size should be > 0

    }

    // No more than one context per type
    it should "have no more than a context of each type simultaneously" ignore {
        context foreach {
              _ foreach {
                  keyVal =>
                      keyVal._2.size should be <= 1
              }
        }
    }

    it should "have fallback species" in {
        mentions foreach {
            m =>
                val c = m.context.getOrElse(Map[String,Seq[String]]())
                c.contains("Species") should be (true)
                c("Species").length should equal (1)
        }
    }
  }

  def differentTypeOfMentionsWithContextBehavior(paperNum:Int) = {
      val mentions = paperAnnotations(paperNum).mentions

      val tbMentions = mentions.filter{
        case m:BioTextBoundMention =>
          if(m.hasContext)
            true
          else
            false
        case _ => false
      }

      val eventMentions = mentions.filter{
        case m:BioEventMention =>
          if(m.hasContext)
            true
          else
            false
        case _ => false
      }

      val relationMentions = mentions.filter{
        case m:BioRelationMention => true
        case _ => false
      }

      info(s"The number of text-bound mentions with context is: ${tbMentions.size}")
      tbMentions.size should be > 0

      info(s"The number of event mentions with context is: ${eventMentions.size}")
      eventMentions.size should be > 0

      // This is "optional" as I believe there's no BioRelationMention rule so far,
      // but if there will be some, it should work as well
      if(relationMentions.nonEmpty) {
        val rmWithContext = relationMentions.filter(_.hasContext)
        info(s"The number of event mentions with context is: ${rmWithContext.size}")
        rmWithContext.size should be > 0
      }
  }

  // def boundedPaddingBehavior(paperNum:Int){
  //   info(s"Testing bounding padding context")
  //
  //   val annotation = paperAnnotations(paperNum)
  //
  //   val friesEntries = annotation.friesEntries
  //   val documents = annotation.documents
  //   val entitiesPerEntry = annotation.entitiesPerEntry
  //
  //   val bound = 5
  //   val boundedPaddingEngine = new BoundedPaddingContext(bound)
  //   boundedPaddingEngine.infer(entitiesPerEntry.flatten)
  //
  //   // No more than $bound repetitions of the same context
  //
  //   it should s"not extend an existing context more than $bound times" in {
  //
  //     val sparseMatrix = boundedPaddingEngine.preprocessedLatentStateMatrix.transpose
  //     val matrix = boundedPaddingEngine.latentStateMatrix.transpose
  //     // Transpose the matrix
  //
  //     // Count the number of zeros from before hitting a one from right to left
  //
  //     val counts = sparseMatrix.map(_.scanRight(0){(step:Boolean, cs:Int) => if(!step) cs+1 else 0}.drop(1))
  //
  //     // Select the indices that are to be checked
  //     val selection = (sparseMatrix zip counts).map{ case (s, c) => (s zip c).zipWithIndex filter {case((value, count), ix) => value && count > bound} map (_._2)}
  //
  //     // check them
  //     selection foreach {
  //       _ foreach {
  //         x =>
  //           val extended = matrix(2).drop(x).take(bound+1).map(if(_) 1 else 0).sum
  //           extended should be <= bound
  //       }
  //     }
  //
  //   }
  // }
  //
  // def bidirectionalPaddingBehavior(paperNum:Int){
  //   info(s"Testing bidirectional bounding padding context")
  //
  //   val annotation = paperAnnotations(paperNum)
  //
  //   val friesEntries = annotation.friesEntries
  //   val documents = annotation.documents
  //   val entitiesPerEntry = annotation.entitiesPerEntry
  //   val paperMentions = annotation.mentions
  //
  //   val boundedPaddingEngine = new BidirectionalPaddingContext(bound=5)
  //   boundedPaddingEngine.infer(entitiesPerEntry.flatten)
  //
  //   // No more than $bound repetitions of the same context
  //   val bound = 10
  //   it should s"not extend an existing context more than $bound times in both directions" in {
  //
  //     val sparseMatrix = boundedPaddingEngine.preprocessedLatentStateMatrix.transpose
  //     val matrix = boundedPaddingEngine.latentStateMatrix.transpose
  //     // Transpose the matrix
  //
  //     // Count the number of zeros from before hitting a one from right to left
  //
  //     val counts = sparseMatrix.map(_.scanRight(0){(step:Boolean, cs:Int) => if(!step) cs+1 else 0}.drop(1))
  //
  //     // Select the indices that are to be checked
  //     val selection = (sparseMatrix zip counts).map{ case (s, c) => (s zip c).zipWithIndex filter {case((value, count), ix) => value && count > bound} map (_._2)}
  //
  //     // check them
  //     selection foreach {
  //       _ foreach {
  //         x =>
  //           val extended = matrix(2).drop(x).take(bound+1).map(if(_) 1 else 0).sum
  //           extended should be <= bound
  //       }
  //     }
  //
  //   }
  // }

  // Tests
  behavior of "PMC2597732.nxml"

  it should behave like contextAssignmentBehavior(1)
  it should behave like differentTypeOfMentionsWithContextBehavior(1)
  // it should behave like boundedPaddingBehavior(1)
  // it should behave like bidirectionalPaddingBehavior(1)

  // behavior of "PMC3189917.nxml"
  //
  // it should behave like contextAssignmentBehavior(2)
  // it should behave like boundedPaddingBehavior(2)
  // it should behave like bidirectionalPaddingBehavior(2)
  //
  // behavior of "PMC1289294.nxml"
  //
  // it should behave like contextAssignmentBehavior(3)
  // it should behave like boundedPaddingBehavior(3)
  // it should behave like bidirectionalPaddingBehavior(3)

}
