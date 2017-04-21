package org.clulab.reach.focusedreading

import breeze.stats.distributions.RandBasis

import scala.util.Random

/**
  * Created by enrique on 18/04/17.
  */
package object reinforcement_learning {
  val randGen = RandBasis.mt0
  val scalaRand = new Random(0)
}
