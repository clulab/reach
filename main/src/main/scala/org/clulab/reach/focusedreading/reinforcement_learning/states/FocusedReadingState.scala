package org.clulab.reach.focusedreading.reinforcement_learning.states

/**
  * Created by enrique on 31/03/17.
  */

object RankBin extends Enumeration {
  val First, Upper, Mid, Bottom = Value

  def toFeatures(b:RankBin.Value, prefix:String):Map[String, Double] = {
    // Get rid of one bin to avoid mulitcolinearity in the design matrix
    val values = RankBin.values.toSeq//.dropRight(1)

    values.map{
      v =>
        val is = if(v == b) 1.0 else 0.0
        (s"$prefix-${v.toString}" -> is)
    }.toMap
  }
}

case class FocusedReadingState(paRank:Double,
                               pbRank:Double,
                               iteration:Int,
                               paQueryLogCount:Int,
                               pbQueryLogCount:Int,
                               sameComponent:Boolean,
                               paIterationIntroduction:Int,
                               pbIterationIntroduction:Int,
                               paUngrounded:Boolean,
                               pbUngrounded:Boolean
                              ) extends State{

  override def hashCode(): Int = {
    s"$paRank-$pbRank-$iteration-$paQueryLogCount-$pbQueryLogCount-$sameComponent-$paIterationIntroduction-$pbIterationIntroduction--$paUngrounded-$pbUngrounded".hashCode
  }

  override def equals(obj: scala.Any): Boolean = {
    if(obj.getClass == this.getClass){
      val that = obj.asInstanceOf[FocusedReadingState]
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

  override def toFeatures():Map[String, Double] = {
    Map(
      "iteration" -> iteration.toDouble,
      "paQueryLogCount" -> paQueryLogCount.toDouble,
      "pbQueryLogCount" -> pbQueryLogCount.toDouble,
      "sameComponent" ->  (sameComponent match{ case true => 1.0; case false => 0.0 }),
      "paIterationIntroduction" -> paIterationIntroduction.toDouble,
      "pbIterationIntroduction" -> pbIterationIntroduction.toDouble,
      "paRank" -> paRank,
      "pbRank" -> pbRank
      //"paUngrounded" -> (paUngrounded match { case true => 1.0; case false => 0.0}),
      //"pbUngrounded" -> (pbUngrounded match { case true => 1.0; case false => 0.0})
    )  //++ RankBin.toFeatures(paRank, "paRank") ++ RankBin.toFeatures(pbRank, "pbRank")
  }
}

object FocusedReadingState{

  val iterationBound = 10

  // Size of the state space
  def cardinality:Int = {
    RankBin.values.size * RankBin.values.size * iterationBound * iterationBound * iterationBound * 2 * iterationBound * iterationBound
  }

//  def enumerate:Iterable[State] = {
//
//    val iterations = 1 to 10
//
//    val states = for{
//      paRank <- RankBin.values;
//      pbRank <- RankBin.values;
//      iteration <- iterations;
//      paQueryLogCount <- iterations;
//      pbQueryLogCount <- iterations;
//      sameComponent <- Seq(true, false);
//      paIterationIntroduction <- iterations;
//      pbIterationIntroduction <- iterations
//    } yield FocusedReadingState(paRank, pbRank, iteration, paQueryLogCount, pbQueryLogCount, sameComponent, paIterationIntroduction, pbIterationIntroduction)
//
//    assert(states.size == cardinality, s"There's a different number of stats than the computed cardinality. States: ${states.size}, Cardinality: $cardinality")
//    states
//  }
}

