package org.clulab.reach.focusedreading.reinforcement_learning

/**
  * Created by enrique on 21/04/17.
  */
object Decays {

  def genericDecay(decreaseFunction:(Double, Int) => Iterable[Double], initial:Double, lowerBound:Double, steps:Int, delay:Int) = {
    assert (initial > lowerBound, "The initial value should be larger than the lower bound")
    assert(delay < steps, "Delay should be less than the number of steps")

    val adjustedSteps = steps - delay
    val quantity = initial - lowerBound

    // Decrease strides
    val decreases = constantDecrease(quantity, adjustedSteps)

    // Delayed schedule
    val delayStream = Stream.fill(delay)(initial)

    // Lower bound tail (infinite)
    val tailStream = Stream.continually(lowerBound)

    // Decrease stream
    val decreaseStream = decreases.scan(initial)((a, b) => a - b )

    // Put them all together
    delayStream ++ decreaseStream ++ tailStream
  }

  def constantDecrease(quantity:Double, steps:Int):Iterable[Double] = Stream.fill(steps)(quantity/steps)

  def constantDecay(initial:Double, lowerBound:Double, steps:Int, delay:Int) = genericDecay(constantDecrease, initial, lowerBound, steps, delay)

  def linearDecay(upperBound:Double, lowerBound:Double, steps:Int, delay:Int) = {
    assert (upperBound > lowerBound, "The initial value should be larger than the lower bound")
    assert(delay < steps, "Delay should be less than the number of steps")

    val adjustedSteps = steps - delay


    // Delayed schedule
    val delayStream = Stream.fill(delay)(upperBound)

    // Lower bound tail (infinite)
    val tailStream = Stream.continually(lowerBound)

    val x0 = 1/upperBound
    val x1 = 1/lowerBound

    val quantity = x1 - x0
    val stride = quantity/adjustedSteps
    
    val domain = Stream.fill(adjustedSteps)(stride).scan(x0)((a, b) => a+b)

    // Decrease stream
    val decreaseStream = domain.map(x => 1/x)

    // Put them all together
    delayStream ++ decreaseStream ++ tailStream
  }

  def exponentialDecay(upperBound:Double, lowerBound:Double, steps:Int, delay:Int) = {
    assert (upperBound > lowerBound, "The initial value should be larger than the lower bound")
    assert(delay < steps, "Delay should be less than the number of steps")

    val adjustedSteps = steps - delay


    // Delayed schedule
    val delayStream = Stream.fill(delay)(upperBound)

    // Lower bound tail (infinite)
    val tailStream = Stream.continually(lowerBound)

    val x0 = math.log(1) - math.log(upperBound)
    val x1 = math.log(1) - math.log(lowerBound)

    val quantity = x1 - x0
    val stride = quantity/adjustedSteps

    val domain = Stream.fill(adjustedSteps)(stride).scan(x0)((a, b) => a+b)

    // Decrease stream
    val decreaseStream = domain.map(x => 1/math.exp(x))

    // Put them all together
    delayStream ++ decreaseStream ++ tailStream
  }

}
