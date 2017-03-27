package org.clulab.reach.focusedreading.reinforcement_learning

/**
  * Created by enrique on 26/03/17.
  */

object RankBin extends Enumeration {
  val First, Upper, Mid, Bottom = Value
}

case class State(paRank:RankBin.Value,
                 pbRank:RankBin.Value,
                 iteration:Int,
                 paQueryLogCount:Int,
                 pbQueryLogCount:Int,
                 sameComponent:Boolean,
                 paIterationIntroduction:Int,
                 pbIterationIntroduction:Int
                ) {

  override def hashCode(): Int = {
    s"$paRank-$pbRank-$iteration-$paQueryLogCount-$pbQueryLogCount-$sameComponent-$paIterationIntroduction-$pbIterationIntroduction".hashCode
  }

  override def equals(obj: scala.Any): Boolean = {
    if(obj.getClass == this.getClass){
      val that = obj.asInstanceOf[State]
      if(paRank == that.paRank
      && pbRank == that.pbRank
      && iteration == that.iteration
      && paQueryLogCount == that.paQueryLogCount
      && pbQueryLogCount == that.pbQueryLogCount
      && sameComponent == that.sameComponent
      && paIterationIntroduction == that.paIterationIntroduction
      && pbIterationIntroduction == that.pbIterationIntroduction)
        true
      else
        false
    }
    else{
      false
    }
  }
}

object State{
  val iterationBound = 10 // Change this is we allow more iterations

  // Size of the state space
  def cardinality:Int = {
    RankBin.values.size * RankBin.values.size * iterationBound * iterationBound * iterationBound * 2 * iterationBound * iterationBound
  }

  def enumerate:Iterable[State] = {

    val iterations = 1 to 10

    val states = for{
      paRank <- RankBin.values;
      pbRank <- RankBin.values;
      iteration <- iterations;
      paQueryLogCount <- iterations;
      pbQueryLogCount <- iterations;
      sameComponent <- Seq(true, false);
      paIterationIntroduction <- iterations;
      pbIterationIntroduction <- iterations
    } yield State(paRank, pbRank, iteration, paQueryLogCount, pbQueryLogCount, sameComponent, paIterationIntroduction, pbIterationIntroduction)

    assert(states.size == cardinality, s"There's a different number of stats than the computed cardinality. States: ${states.size}, Cardinality: $cardinality")
    states
  }
}