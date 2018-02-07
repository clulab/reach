package org.clulab.reach

import com.typesafe.config.ConfigFactory
import org.clulab.reach.context._
import org.clulab.reach.mentions._
import org.clulab.reach.grounding._
import org.scalatest.{Matchers, FlatSpec}
import scala.util.Try                       // ignore IntelliJ: THIS IS USED!
import TestUtils._

/**
  * Unit tests of the configuration factory.
  */
class TestConfigFactory extends FlatSpec with Matchers {

  behavior of "user.home"
  
  it should "be defined" in {
    val home = sys.props.get("user.home")
    
    home.isDefined should be (true)
  }

  behavior of "ConfigFactory"
  
  it should "load in general" in {
    val config = ConfigFactory.load()
  }

  it should "load without ${HOME}" in {
    val home = sys.env.get("HOME")
    val withoutHome = home == None
    
    if (withoutHome) {
      val config = ConfigFactory.load()
    }
  }

}
