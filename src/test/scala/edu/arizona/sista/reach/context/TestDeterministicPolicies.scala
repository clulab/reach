package edu.arizona.sista.reach.context

import io.Source
import scala.collection.mutable
import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.reach.nxml.NxmlReader
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.ReachSystem

trait Fixtures {
  // Set up the fixtures
  def nxml1 = Source.fromURL(getClass.getResource("/inputs/nxml/PMC2597732.nxml")).mkString
  def nxml2 = Source.fromURL(getClass.getResource("/inputs/nxml/PMC3441633.nxml")).mkString
  def nxml3 = Source.fromURL(getClass.getResource("/inputs/nxml/PMC1289294.nxml")).mkString

  val reader = new NxmlReader
  val reachSystem = new ReachSystem
  /////////
}

class DeterministicPoliciesTests extends FlatSpec with Matchers with Fixtures {

  def contextAssignmentBehavior(nxml:String){
    info("Testing context assignment")
    val entries = reader.readNxml(nxml, nxml)

    val mentions:Seq[BioEventMention] = reachSystem.extractFrom(entries).filter{
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
    it should "have no more than a context of each type simultaneously" in {
        context foreach {
              _ foreach {
                  keyVal =>
                      keyVal._2.size should be <= 1
              }
        }
    }
  }

  def boundedPaddingBehavior(nxml:String){
    info(s"Testing bounding padding context")
    // Extract context for the sentences of a doc, not to the attached mentions
    val friesEntries = reader.readNxml(nxml, "")
    val documents = friesEntries map (e => reachSystem.mkDoc(e.text, e.name, e.chunkId))
    val entitiesPerEntry =  for (doc <- documents) yield reachSystem.extractEntitiesFrom(doc)


    val boundedPaddingEngine = new BoundedPaddingContext
    boundedPaddingEngine.infer(friesEntries, documents, entitiesPerEntry)

    // No more than $bound repetitions of the same context
    val bound = 5
    it should s"not extend an existing context more than $bound times" in {

      val sparseMatrix = boundedPaddingEngine.preprocessedLatentStateMatrix.transpose
      val matrix = boundedPaddingEngine.latentStateMatrix.transpose
      // Transpose the matrix

      // Count the number of zeros from before hitting a one from right to left

      val counts = sparseMatrix.map(_.scanRight(0){(step:Boolean, cs:Int) => if(!step) cs+1 else 0}.drop(1))

      // Select the indices that are to be checked
      val selection = (sparseMatrix zip counts).map{ case (s, c) => (s zip c).zipWithIndex filter {case((value, count), ix) => value && count > bound} map (_._2)}

      // check them
      selection foreach {
        _ foreach {
          x =>
            val extended = matrix(2).drop(x).take(bound+1).map(if(_) 1 else 0).sum
            extended should be <= bound
        }
      }

    }
  }

  // Tests
  behavior of "PMC2597732.nxml"

  it should behave like contextAssignmentBehavior(nxml1)
  it should behave like boundedPaddingBehavior(nxml1)

  behavior of "PMC3441633.nxml"

  it should behave like contextAssignmentBehavior(nxml2)
  it should behave like boundedPaddingBehavior(nxml2)

  behavior of "PMC1289294.nxml"

  it should behave like contextAssignmentBehavior(nxml3)
  it should behave like boundedPaddingBehavior(nxml3)

}
