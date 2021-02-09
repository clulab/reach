package org.clulab.reach.context

import org.clulab.processors.Document
import org.clulab.reach.context.train.dataset.ManuallyAnnotatedData
import org.clulab.utils.Serializer

/**
  * Utility functions shared by the scripts
  */
package object utils {
  def readSerializedPaperAnnotations(path:String):Map[String, ManuallyAnnotatedData] = {
    val data = Serializer.load[Map[String, ManuallyAnnotatedData]](path)
    data
  }

  def readSerializedDocument(path:String):Map[String, Document] = {
    val data = Serializer.load[Seq[(String, (Seq[PaperExtraction], Document))]](path)
    data.toMap.mapValues(_._2)
  }

  def readSerializedExtractions(path:String):Map[String, Seq[PaperExtraction]] = {
    val data = Serializer.load[Seq[(String, (Seq[PaperExtraction], Document))]](path)
    data.toMap.mapValues(_._1)
  }

  def readSerializedPairs(path:String):Map[String, Iterable[Pair]] = {
    Serializer.load[Map[String, Iterable[Pair]]](path)
  }

  def getEvents(data:Seq[PaperExtraction]):Seq[PaperExtraction] = data filter (_.grounding == "Event")

  def generateValidContextIds(annotations:Map[String, ManuallyAnnotatedData]):Set[String] = {
    annotations.values.flatMap(_.annotations.values).flatten.toSet
  }
}