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
  def nxml2 = Source.fromURL(getClass.getResource("/inputs/nxml/PMC534114.nxml")).mkString
  def artificialNxml = Source.fromURL(getClass.getResource("/inputs/nxml/ContextTests.nxml")).mkString

  val reader = new NxmlReader
  val reachSystem = new ReachSystem
  /////////
}

class DeterministicPoliciesTests extends FlatSpec with Matchers with Fixtures {

  def contextAssignment(nxml:String){

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

  // Tests
  behavior of "PMC2597732.nxml"

  it should behave like contextAssignment(nxml1)

  // behavior of "PMC534114.nxml"
  //
  // it should behave like contextAssignment(nxml2)

  behavior of "Context tests"

  // Extract context for the sentences of a doc, not to the attached mentions
  val friesEntries = reader.readNxml(nxml1, "")
  val documents = friesEntries map reachSystem.mkDoc
  val entitiesPerEntry =  for (doc <- documents) yield reachSystem.extractEntitiesFrom(doc)

  behavior of "Bounding padding context"
  val boundedPaddingEngine = new BoundedPaddingContext
  boundedPaddingEngine.infer(friesEntries, documents, entitiesPerEntry)

  // No more than 5 repetitions of the same context
  it should "not extend an existing context more than 5 times" in {

    val matrix = boundedPaddingEngine.latentStateMatrix


    val x = List.fill(matrix(0).size)(0)
    info(s"Vector dimensions: ${matrix.size} x ${x.size}")
    info(s"${matrix map (_.mkString(" ")) mkString ("\n")}")

    val accumulators:List[Int] = matrix.foldRight(x){
      (current:Seq[Boolean], prev:List[Int]) =>
        prev zip current map {
          case (p, c) => if(!c) 0 else p + 1
        }
    }

    val max = accumulators.max
    info(s"The max span of a context is of $max")
    max should be <= 5
  }

  // Mention where context starts

  ////////
}
