package edu.arizona.sista.reach.fragmenter

import org.biopax.paxtools.model.level3._

object Labeler {
  def conversionLabels(conversion: Conversion): Set[String] = {
    val labelsMatched = for {
      name <- conversion.allNames
      (label, triggers) <- labelTriggersMap
      regex = regexFromTriggers(triggers)
    } yield if (regex.findFirstIn(name).isDefined) Some(label) else None
    labelsMatched.flatten
  }

  def regexFromTriggers(triggers: List[String]) = s"(?i)\\b(?:${triggers.mkString("|")})\\b".r

  val labelTriggersMap = Map(
    "Acetylation" -> List("acetylation"),
    "Activation" -> List("activation", "activates"),
    "Binding" -> List("bind", "binds", "binding"),
    "Conversion" -> List("conversion", "transforms"),
    "Deacetylation" -> List("deacetylation"),
    "Degradation" -> List("degradation", "is degraded", "degrades"),
    "Demethylation" -> List("demethylation", "demethylates"),
    "Dephosphorylation" -> List("dephosphorylation", "dephosphorylates"),
    "Deubiquitination" -> List("deubiquitination", "deubiquitinates"),
    "Dissociation" -> List("dissociation", "dissociates"),
    "Gene_expression" -> List("expression"),
    "Hydroxylation" -> List("hydroxylation", "hydroxylates", "is hydroxylated"),
    "Inactivation" -> List("inactivation", "inactivates"),
    "Localization" -> List("localization"),
    "Methylation" -> List("methylation", "is methylated"),  // trimethylates?
    "Negative_regulation" -> List("negative regulation", "negatively regulates", "downregulation"),
    //"Pathway" -> List(),
    "Phosphorylation" -> List("phosphorylation", "phosphorylates", "autophosphorylation", "is phosphorylated"),
    "Positive_regulation" -> List("positive regulation", "positively regulates"),
    "Regulation" -> List("regulation"),
    "Transcription" -> List("transcription"),
    "Translation" -> List("translation"),
    "Transport" -> List("translocation", "transports", "transport"),
    "Ubiquitination" -> List("ubiquitination", "ubiquitinates")
  )

  def isPhosphorylation(conversion: Conversion) = {
    val phosphoFeats = conversion.featureDiff.values flatMap (_("added")) map (_.toString) filter (_ contains "phospho")
    !phosphoFeats.isEmpty
  }

  def isDephosphorylation(conversion: Conversion) = {
    val dephosphoFeats = conversion.featureDiff.values flatMap (_("removed")) map (_.toString) filter (_ contains "phospho")
    !dephosphoFeats.isEmpty
  }

  def isTransport(conversion: Conversion) = {
    val locFeats = conversion.locationDiff.values filter (v => !v("added").isEmpty && !v("removed").isEmpty)
    !locFeats.isEmpty
  }

  def isBinding(conversion: Conversion) = {
    conversion.input.size > 1 && conversion.output.size == 1 && conversion.output.head.isInstanceOf[Complex]
  }

}
