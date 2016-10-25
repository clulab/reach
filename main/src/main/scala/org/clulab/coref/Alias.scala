package org.clulab.coref

import org.clulab.reach.mentions._
import org.clulab.processors.Document

object Alias {
  def canonizeAliases(mentions: Seq[BioMention], doc: Document): Seq[BioMention] = {

    val (aliasRelations, entities) = mentions.partition(m => m matches "Alias")

    val sourceToTarget = for {
      ar <- aliasRelations
      source = ar.namedArguments("aliasSource")
      targets = ar.namedArguments("aliasTarget")
      if source.nonEmpty & targets.nonEmpty
    } yield source.get.head -> targets.get

    val aliasByLabel = sourceToTarget.groupBy{ case (src, tgts) => src.labels }

    val targetToSource = (for {
      (src, tgts) <- sourceToTarget
      tgt <- tgts
    } yield tgt -> src).toMap

    val newMentions = for {
      (label, aliases) <- aliasByLabel
    } yield {
      val finder = new InstanceFinder(label, "aliasRecognizer")
      aliases.foreach{ case (src, tgts) => tgts.foreach(tgt => finder.add(tgt.text.split("\\s+"))) }
      finder.findAllIn(doc)
    }

    entities ++ newMentions.flatten.map(_.toBioMention)
  }

  def canonizeAliases(mentions: Seq[Seq[BioMention]], docs: Seq[Document]): Seq[Seq[BioMention]] = {

    val (aliasRelations, entities) = mentions.map(_.partition(m => m matches "Alias")).unzip

    val sourceToTarget = for {
      ar <- aliasRelations.flatten
      source = ar.namedArguments("aliasSource")
      targets = ar.namedArguments("aliasTarget")
      if source.nonEmpty & targets.nonEmpty
    } yield source.get.head -> targets.get

    val aliasByLabel = sourceToTarget.groupBy{ case (src, tgts) => src.labels }

    val targetToSource = (for {
      (src, tgts) <- sourceToTarget
      tgt <- tgts
    } yield tgt -> src).toMap

    val newMentions = for {
      doc <- docs
    } yield {
      val newEntities = for {
        (label, aliases) <- aliasByLabel
      } yield {
        val finder = new InstanceFinder(label, "aliasRecognizer")
        aliases.foreach { case (src, tgts) => tgts.foreach(tgt => finder.add(tgt.text.split("\\s+"))) }
        finder.findAllIn(doc)
      }
      newEntities.flatten.toSeq.map(_.toBioMention)
    }

    val zipped = for (i <- entities.indices) yield entities(i) ++ newMentions(i)

    zipped
  }
}