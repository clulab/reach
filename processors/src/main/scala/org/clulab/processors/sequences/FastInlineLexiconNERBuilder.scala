package org.clulab.processors.sequences

import org.clulab.sequences.{CombinedLexiconNER, FastLexiconNERBuilder, LexicalVariations, LexiconNER}
import org.clulab.struct.{CompactLexiconNER, EntityValidator}
import org.clulab.utils.Files.loadStreamFromClasspath
import org.clulab.utils.Serializer

class FastInlineLexiconNERBuilder(caseInsensitiveMatching:Boolean) extends FastLexiconNERBuilder(caseInsensitiveMatching) {

  def build(kbs: Map[String, Seq[String]], overrideKBs: Option[Seq[String]], entityValidator: EntityValidator, lexicalVariationEngine: LexicalVariations, useLemmasForMatching: Boolean): LexiconNER = {
    logger.info("Beginning to load the KBs for the rule-based NER...")
    val buildState = new BuildState(lexicalVariationEngine)
    addOverrideKBs(overrideKBs, buildState) // first so they take priority during matching
    addInlineStandardKBs(kbs, buildState)
    logger.info("KB loading completed.")

    val labels = buildState.labelToIndex.toArray.sortBy(_._2).map(_._1)

    if (LexiconNER.USE_COMPACT)
      CompactLexiconNER(buildState.intHashTrie, labels, buildState.knownCaseInsensitives.toSet, useLemmasForMatching, entityValidator)
    else
      new CombinedLexiconNER(buildState.intHashTrie, labels, buildState.knownCaseInsensitives.toSet, useLemmasForMatching, entityValidator)
  }

  private def addInlineStandardKBs(kbs: Map[String, Seq[String]], buildState: BuildState): Unit = {
    def addLine(inputLine: String, label: String): Unit = {
      val line = inputLine.trim
      if (!line.startsWith("#")) {
        val tokens = line.split("\\s+")
        buildState.addWithLexicalVariations(label, tokens)
      }
    }


    kbs.foreach { case (label, kb) =>
        val kb = kbs(label)
        val beforeCount = buildState.getCount
        kb foreach { line =>
            addLine(line, label)
        }
        var afterCount = buildState.getCount
        logger.info(s"Loaded matcher for label $label. The number of entries added to the first layer was ${afterCount - beforeCount}.")
    }
  }

  private def addOverrideKBs(overrideKBs: Option[Seq[String]], buildState: BuildState): Unit = {
    // in these KBs, the name of the entity must be the first token, and the label must be the last
    //   (tokenization is performed around TABs here)
    def addLine(inputLine: String): Unit = {
      val line = inputLine.trim
      if (!line.startsWith("#")) { // skip comments starting with #
        val blocks = line.split("\t")
        if (blocks.size >= 2) {
          val entity = blocks.head   // grab the text of the named entity
          val label = blocks.last    // grab the label of the named entity
          val tokens = entity.split("\\s+")
          buildState.addWithLexicalVariations(label, tokens)
        }
      }
    }

    overrideKBs.getOrElse(Seq.empty).foreach { okb =>
      val beforeCount = buildState.getCount
      Serializer.using(loadStreamFromClasspath(okb)) { reader =>
        reader.lines.forEach(toJavaConsumer[String] { line: String =>
          addLine(line)
        })
      }
      var afterCount = buildState.getCount
      logger.info(s"Loaded OVERRIDE matchers for all labels.  The number of entries added to the first layer was ${afterCount - beforeCount}.")
    }
  }
}
