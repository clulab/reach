package org.clulab.processors.bionlp.ner

import com.typesafe.config._
import ai.lum.common.ConfigUtils._
import org.clulab.sequences.LexiconNER
import org.slf4j.LoggerFactory

import org.clulab.processors.bionlp.{BioLexicalVariations, BioLexiconEntityValidator}
import org.clulab.sequences.ResourceOverrideKbSource

class KBLoader

/**
  * Loads the KBs from bioresources under org/clulab/reach/kb/ner
  * These must be generated offline by KBGenerator; see bioresources/ner_kb.sh
  * User: mihais. 2/7/16.
  * Last Modified: Update to use external configuration file.
  */
object KBLoader {
  val config: Config = ConfigFactory.load()         // load the configuration file

  private val logger = LoggerFactory.getLogger(classOf[KBLoader])

  // logger.debug(s"KBLoader.init): RULE_NER_KBS=$RULE_NER_KBS")

  /** List of KB override files to be used. */
  val NER_OVERRIDE_KBS: List[String] =
    if (config.hasPath("kbloader.overrides")) config[List[String]]("kbloader.overrides")
    else List.empty[String]
  // logger.debug(s"KBLoader.init): NER_OVERRIDE_KBS=$NER_OVERRIDE_KBS")

  /** These must be KBs BEFORE KBGenerator converts them to NER-ready, because
    * the files under kb/ner are post tokenization. */
  private val unslashable =
    if (config.hasPath("kbloader.unslashables")) config[List[String]]("kbloader.unslashables")
    else List.empty[String]
  val UNSLASHABLE_TOKENS_KBS: List[String] = NER_OVERRIDE_KBS ++ unslashable
  // logger.debug(s"KBLoader.init): UNSLASHABLE_TOKENS_KBS=$UNSLASHABLE_TOKENS_KBS")

  /** A horrible hack to keep track of entities that should not be labeled when in
    * lower case, or upper initial case. */
  val stopListFile: Option[String] =
    if (config.hasPath("kbloader.stopListFile")) Option(config[String]("kbloader.stopListFile"))
    else None

  val serNerModel: Option[String] =
    if(config.hasPath("kbloader.nerSerModel")) Option(config[String]("kbloader.nerSerModel"))
    else None

  // Load the rule NER just once, so multiple processors can share it
  lazy val ruleNerSingleton: LexiconNER = {
    logger.debug("Loading LexiconNER from knowledge bases...")
    val caseInsensitiveMatching = true
    val standardKbSources = KBGenerator.processKBFiles(caseInsensitiveMatching)
    val overrideKbSources = NER_OVERRIDE_KBS.map(new ResourceOverrideKbSource(_))
    val ruleNerSingleton = LexiconNER(
      standardKbSources,
      Some(overrideKbSources), // allow overriding for some key entities
      new BioLexicalVariations,
      new BioLexiconEntityValidator,
      useLemmasForMatching = false,
      defaultCaseInsensitive = caseInsensitiveMatching
    )
    logger.debug("Completed NER loading.")
    ruleNerSingleton
  }
}
