package org.clulab.reach.context.scripts

import org.clulab.learning.{LinearSVMClassifier, RVFDataset, RVFDatum}
import org.clulab.struct.Counter

object SVMTest extends App{

  def mkRVFDatum[L](label:L, features:List[String]):RVFDatum[L, String] = {
    val c = new Counter[String]
    for(f <- features) c.incrementCount(f)
    new RVFDatum[L, String](label, c)
  }


  //val classifier = new LibSVMClassifier[String, String](LinearKernel, C = 1, cacheSize = 100)
  val classifier2 = new LinearSVMClassifier[String,String](C = 1, eps = 0.001, bias = false)

  val dataset = new RVFDataset[String, String]()

  val d1 = mkRVFDatum("+", List("good", "great", "good"))
  val d2 = mkRVFDatum("-", List("bad", "awful"))
  val d3 = mkRVFDatum("~", List("meh", "soso"))

  dataset += d1
  dataset += d2
  dataset += d3
  //classifier.train(dataset)
  classifier2.train(dataset)

  val dn = mkRVFDatum("+", List("good", "great", "bad", "new"))
  /*println(classifier.classOf(d1))
  println(classifier.classOf(d2))
  println(classifier.classOf(dn))*/
  println(classifier2.classOf(d1))
  println(classifier2.classOf(d2))
  println(classifier2.classOf(dn))

}
