package edu.arizona.sista.reach

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.processors.{ Document, Sentence }

package object display {

  def displayMentions(mentions: Seq[Mention], doc: Document): Unit = {
    val mentionsBySentence = mentions groupBy (_.sentence) mapValues (_.sortBy(_.start)) withDefaultValue Nil
    for ((s, i) <- doc.sentences.zipWithIndex) {
      println(s"sentence #$i")
      println(s.getSentenceText)
      println("Tokens: " + (s.words.indices, s.words, s.tags.get).zipped.mkString(", "))
      printSyntacticDependencies(s)
      println

      val sortedMentions = mentionsBySentence(i).filter(!_.toCorefMention.isGeneric).sortBy(_.label)
      val (events, entities) = sortedMentions.partition(_ matches "Event")
      val (tbs, rels) = entities.partition(_.isInstanceOf[TextBoundMention])
      val sortedEntities = tbs ++ rels.sortBy(_.label)
      println("entities:")
      sortedEntities foreach displayMention

      println
      println("events:")
      events foreach displayMention
      println("=" * 50)
    }
  }

  def printSyntacticDependencies(s:Sentence): Unit = {
    if(s.dependencies.isDefined) {
      println(s.dependencies.get.toString)
    }
  }

  def displayMention(mention: Mention) {
    val printMention = mention.antecedentOrElse(mention)
    val boundary = s"\t${"-" * 30}"
    println(s"mention text: ${printMention.text}")
    println(printMention.labels)
    println(boundary)
    println(s"\tRule => ${printMention.foundBy}")
    val mentionType = printMention.getClass.toString.split("""\.""").last
    println(s"\tType => $mentionType")
    println(boundary)
    printMention match {
      case tb: BioTextBoundMention =>
        println(s"\t${tb.asInstanceOf[Display].displayLabel}|${tb.labels} => ${tb.text}")
        if (tb.isGrounded) println(s"\txref: ${tb.xref.get}")
        displayModifications(tb)
      case em: BioEventMention =>
        displayModifications(em)
        println(boundary)
        println(s"\ttrigger => ${em.trigger.text}")
        displayArguments(em)
      case rel: BioRelationMention =>
        displayModifications(rel)
        println(boundary)
        displayArguments(rel)
      case _ => ()
    }
    println(s"$boundary\n")
  }


  def displayArguments(b: BioMention): Unit = {
    b.arguments foreach {
      case (k, vs) =>
        vs foreach { v =>
          val vm = v.antecedentOrElse(v.toCorefMention)
          println(s"\t$k (${vm.labels}) => ${vm.text}")
          displayModifications(vm)
        }
    }
  }

  def displayModifications(b: BioMention, level:Int = 0): Unit = {
    val indent = "\t" * level
    b.modifications.size match {
      case 1 =>
        println(s"$indent\twith 1 modification =>")
      case x if x > 1 =>
        println(s"$indent\twith $x modifications =>")
      case _ => ()
    }
    b.modifications foreach {
      case Negation(evidence) =>
        println(s"""$indent\t\tNegated by \"${evidence.text}\"""")
      case Hypothesis(evidence) =>
        println(s"""$indent\t\tHypothesis by \"${evidence.text}\"""")
      case Mutant(evidence) =>
        println(s"""$indent\t\tMutant by \"${evidence.text}\"""")
      case PTM(mod, evidence, site) =>
        val siteText = if (site.nonEmpty) {s" @ ${site.get}"} else ""
        val evidenceText = if (evidence.nonEmpty) {s""" based on \"${evidence.get.text}\""""} else ""
        println(s"""$indent\t\t$PTM = \"$mod\"$siteText$evidenceText""")
      case EventSite(site) =>
        println(s"""$indent\t\twith Site \"${site.text}\"""")
      case _ => ()
    }
  }

  def cleanVerbose(s:String):String = {
    val spaceBefore = """\s+([ .,;!?%)\]}>])""".r
    val firstStep = spaceBefore replaceAllIn (s, m => m group 1)
    val spaceAfter = """([(\[{])\s+""".r
    spaceAfter replaceAllIn (firstStep, m => m group 1)
  }

}
