package org.clulab.reach

import org.clulab.odin._
import org.clulab.reach.mentions._
import org.clulab.processors.{Document, Sentence}

import scala.util.matching.Regex


package object display {

  val WHITESPACE = new Regex("^\\s+")

  def summarizeMentions(mentions: Seq[Mention], doc: Document): String = {

    val mentionsBySentence = mentions groupBy (_.sentence) mapValues (_.sortBy(_.start)) withDefaultValue Nil

    val sentenceSummaries = for ((s, i) <- doc.sentences.zipWithIndex) yield {

      val sortedMentions = mentionsBySentence(i).filter(!_.toCorefMention.isGeneric).sortBy(_.label)
      val (events, entities) = sortedMentions.partition(_ matches "Event")
      val (tbs, rels) = entities.partition(_.isInstanceOf[TextBoundMention])
      val sortedEntities = tbs ++ rels.sortBy(_.label)
      val entitySummaries = sortedEntities map summarizeMention
      val eventSummaries = events map summarizeMention
      val boundary = "=" * 50

      s"""
         |sentence #$i
         |TEXT:   ${s.getSentenceText}
         |TOKENS: ${(s.words.indices, s.words, s.tags.get).zipped.mkString(", ")}
         |ENTITY LABELS: ${(s.words, s.entities.get).zipped.mkString(", ")}
         |${syntacticDependenciesToString(s)}
         |ENTITIES: ${sortedEntities.size}
         |${entitySummaries.mkString("\n")}
         |EVENTS:   ${events.size}
         |${eventSummaries.mkString("\n")}
         |$boundary
       """.stripMargin
    }

    sentenceSummaries.mkString("\n")
  }

  def displayMentions(mentions: Seq[Mention], doc: Document): Unit = {
    println(summarizeMentions(mentions, doc))
  }

  def printSyntacticDependencies(s:Sentence): Unit = {
    println(syntacticDependenciesToString(s))
  }

  def syntacticDependenciesToString(s:Sentence): String = {

    val lemmas = s.lemmas.get.mkString(" ")

    val summaryOfDependencies = s.dependencies match {
      case Some(deps) => deps.toString
      case None => ""
    }

    s"""
       |LEMMAS: $lemmas
       |$summaryOfDependencies
     """.stripMargin
  }

  /** Remove entries containing only whitespace */
  private def filterEntries(entries: List[String]): String = entries.filterNot{ entry =>
    WHITESPACE.pattern.matcher(entry).matches
  }.mkString("\n")

  def summarizeMention(mention: Mention): String = {
    val mentionForDisplay = mention.antecedentOrElse(mention).toBioMention
    val boundary = s"\t${"-" * 30}"
    val mentionType = mentionForDisplay.getClass.toString.split("""\.""").last

    val summaryOfMods = summarizeModifications(mentionForDisplay)
    val summaryOfArgs = summarizeArguments(mentionForDisplay)
    val summaryOfContext = summarizeContext(mentionForDisplay)

    val mentionContents = mentionForDisplay match {
      case tb: BioTextBoundMention =>
         List(
           s"""\tGROUNDING: ${if (tb.isGrounded) tb.grounding.get else "NONE"}""",
           summaryOfMods,
           summaryOfContext
         )
      case em: BioEventMention =>
        List(
          summaryOfMods,
          s"$boundary",
          s"\tTRIGGER => ${em.trigger.text}",
          summaryOfArgs,
          summaryOfContext
        )
      case rel: BioRelationMention =>
        List(
          summaryOfMods,
          boundary,
          summaryOfArgs,
          summaryOfContext
        )
      case _ => Nil
    }

      s"""
        |MENTION TEXT:  ${mentionForDisplay.text}
        |LABELS:        ${mentionForDisplay.labels}
        |DISPLAY LABEL: ${mentionForDisplay.displayLabel}
        |$boundary
        |\tRULE => ${mentionForDisplay.foundBy}
        |\tTYPE => $mentionType
        |$boundary
        |${filterEntries(mentionContents).replaceFirst("\\s+$", "")}
        |$boundary
        |
        |""".stripMargin
  }

  def displayMention(mention: Mention): Unit = {
    println(summarizeMention(mention))
  }

  def summarizeArguments(b: BioMention): String = {
    val argSummaries = for {
      (k, vs) <- b.arguments
      v <- vs
      vm = v.antecedentOrElse(v.toCorefMention)
    } yield s"\t$k (${vm.labels}) => ${vm.text}${summarizeModifications(vm)}"
    argSummaries.mkString("\n")
  }

  def displayArguments(b: BioMention): Unit = {
    println(summarizeArguments(b))
  }

  def summarizeContext(b: BioMention): String = b.contextOpt match {
    case Some(context) =>
      val contextSummary = for {
        (k, vs) <- context
      } yield s"\tCONTEXT: $k => $vs"
      contextSummary.mkString("\n")
    case None => s"CONTEXT: NONE"
  }

  def displayContext(b: BioMention): Unit = {
    println(summarizeContext(b))
  }

  def summarizeModifications(b: BioMention, level:Int = 0): String = {
    val indent = "\t" * level
    val modCountString = b.modifications.size match {
      case 1 =>
        s"\n$indent\twith 1 modification =>"
      case x if x > 1 =>
        s"\n$indent\twith $x modifications =>"
      case _ => ""
    }
    val summaryOfMods = b.modifications map {
      case Negation(evidence) =>
        s"""$indent\t\tNegated by \"${evidence.text}\""""
      case KDtrigger(evidence) =>
        s"""$indent\t\tKnockdown Triggered by \"${evidence.text}\""""
      case KOtrigger(evidence) =>
        s"""$indent\t\tKnockout Triggered by \"${evidence.text}\""""
      case DNtrigger(evidence) =>
        s"""$indent\t\tDominant Negative Triggered by \"${evidence.text}\""""
      case OEtrigger(evidence) =>
        s"""$indent\t\tOverexpression Triggered by \"${evidence.text}\""""
      case CHEMtrigger(evidence) =>
        s"""$indent\t\tChemical Inhibition Triggered by \"${evidence.text}\""""
      case UnassignedTrigger(evidence) =>
        s"""$indent\t\tUnassigned Modification Triggered by \"${evidence.text}\""""

      case Hypothesis(evidence) =>
        s"""$indent\t\tHypothesis by \"${evidence.text}\""""
      case Mutant(evidence, foundBy) =>
        s"""$indent\t\t${evidence.label} by \"${evidence.text}\"
           |$indent\t\t\tMutation rule: ${evidence.foundBy}
           |$indent\t\t\tMutation attachment rule: $foundBy""".stripMargin
      case PTM(mod, evidence, site, negated) =>
        val siteText = if (site.nonEmpty) {s" @ ${site.get.text}"} else ""
        val evidenceText = if (evidence.nonEmpty) {s""" based on \"${evidence.get.text}\""""} else ""
        s"""$indent\t\t$PTM (negated=$negated) = \"$mod\"$siteText$evidenceText"""
      case EventSite(site) =>
        s"""$indent\t\twith Site \"${site.text}\""""
      case _ => ""
    }

    val entries: List[String] = List(modCountString) ++ summaryOfMods
    filterEntries(entries)
  }

  def displayModifications(b: BioMention, level: Int = 0): Unit = {
    println(summarizeModifications(b, level))
  }

  def cleanVerbose(s:String):String = {
    val spaceBefore = """\s+([ .,;!?%)\]}>])""".r
    val firstStep = spaceBefore replaceAllIn (s, m => m group 1)
    val spaceAfter = """([(\[{])\s+""".r
    spaceAfter replaceAllIn (firstStep, m => m group 1)
  }

}
