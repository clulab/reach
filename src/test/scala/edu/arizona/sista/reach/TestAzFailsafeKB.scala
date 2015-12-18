package edu.arizona.sista.reach

import org.scalatest.{Matchers, FlatSpec}
import TestUtils._
import edu.arizona.sista.reach.grounding._

/**
  * Unit tests to ensure the AZ fail safe KB is working for grounding.
  *   Written by: Tom Hicks. 11/10/2015.
  *   Last Modified: Update for new/refactored resolves.
  */
class TestAzFailsafeKB extends FlatSpec with Matchers {

  // Tests of AZ Failsafe KB
  val kbAZ = new AzFailsafeKBML

  "AZ Failsafe KB" should "return new UAZ ID for new resolve, then repeat it" in {
    val x3 = kbAZ.resolve("XXX")
    (x3.isDefined) should be (true)
    val xxx = x3.get
    (kbAZ.resolve("XXX").get.id == xxx.id) should be (true)
    (kbAZ.resolve("xxx").get.id == xxx.id) should be (true)
    (kbAZ.resolve("XXXX").get.id == xxx.id) should be (false) // 4 Xs
    (kbAZ.resolve("xxxx").get.id == xxx.id) should be (false) // 4 xs
  }

  "AZ Failsafe KB" should "return new UAZ ID for new resolve and resolveHuman" in {
    val x3 = kbAZ.resolve("XXX")
    (x3.isDefined) should be (true)
    val xxx = x3.get
    (kbAZ.resolveHuman("XXX").isDefined) should be (true)
    (kbAZ.resolveHuman("XXX").get.id == xxx.id) should be (true)
    (kbAZ.resolveHuman("xxx").isDefined) should be (true)
    (kbAZ.resolveHuman("xxx").get.id == xxx.id) should be (true)
  }

  "AZ Failsafe KB" should "return new UAZ ID for new resolve and resolveByASpecies" in {
    val x3 = kbAZ.resolve("XXX")
    (x3.isDefined) should be (true)
    val xxx = x3.get
    (kbAZ.resolveByASpecies("XXX", "giraffe").isDefined) should be (true)
    (kbAZ.resolveByASpecies("XXX", "giraffe").get.id == xxx.id) should be (true)
    (kbAZ.resolveByASpecies("xxx", "giraffe").isDefined) should be (true)
    (kbAZ.resolveByASpecies("xxx", "giraffe").get.id == xxx.id) should be (true)
  }

  "AZ Failsafe KB" should "return new UAZ ID for new resolve and resolveBySpecies" in {
    val x3 = kbAZ.resolve("XXX")
    (x3.isDefined) should be (true)
    val xxx = x3.get
    (kbAZ.resolveBySpecies("XXX", Set("giraffe")).isDefined) should be (true)
    (kbAZ.resolveBySpecies("XXX", Set("giraffe")).get.head.id == xxx.id) should be (true)
    (kbAZ.resolveBySpecies("XXX", Set("human", "mouse")).isDefined) should be (true)
    (kbAZ.resolveBySpecies("XXX", Set("human", "mouse")).get.head.id == xxx.id) should be (true)
    (kbAZ.resolveBySpecies("xxx", Set("giraffe")).isDefined) should be (true)
    (kbAZ.resolveBySpecies("xxx", Set("giraffe")).get.head.id == xxx.id) should be (true)
    (kbAZ.resolveBySpecies("xxx", Set("human", "mouse")).isDefined) should be (true)
    (kbAZ.resolveBySpecies("xxx", Set("human", "mouse")).get.head.id == xxx.id) should be (true)
  }

  "AZ Failsafe KB" should "return new UAZ ID for new resolve and resolveNoSpecies" in {
    val x3 = kbAZ.resolve("XXX")
    (x3.isDefined) should be (true)
    val xxx = x3.get
    (kbAZ.resolveNoSpecies("XXX").get.id == xxx.id) should be (true)
    (kbAZ.resolveNoSpecies("xxx").get.id == xxx.id) should be (true)
    (kbAZ.resolveNoSpecies("XXXX").get.id == xxx.id) should be (false) // 4 Xs
    (kbAZ.resolveNoSpecies("xxxx").get.id == xxx.id) should be (false) // 4 xs
  }

}
