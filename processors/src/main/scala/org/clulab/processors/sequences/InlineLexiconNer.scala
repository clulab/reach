package org.clulab.processors.sequences

import org.clulab.sequences.LexiconNER.{OVERRIDE_ENTITY_VALIDATOR, USE_FAST}
import org.clulab.sequences.{FastLexiconNERBuilder, LexicalVariations, LexiconNER, SlowLexiconNERBuilder}
import org.clulab.struct.EntityValidator

object InlineLexiconNer {
  def apply(kbs: Seq[(String, Seq[String])], overrideKBs: Option[Seq[String]],
            entityValidator: EntityValidator,
            lexicalVariationEngine: LexicalVariations,
            useLemmasForMatching: Boolean,
            caseInsensitiveMatching: Boolean): LexiconNER = {
    val newEntityValidator =
      if (OVERRIDE_ENTITY_VALIDATOR) EntityValidator.TRUE_VALIDATOR
      else  entityValidator

//    if (USE_FAST)
      new FastInlineLexiconNERBuilder(caseInsensitiveMatching).buildWithOrder(kbs, overrideKBs, newEntityValidator,
        lexicalVariationEngine, useLemmasForMatching)
//    else
//      new SlowLexiconNERBuilder(caseInsensitiveMatching).build(kbs, overrideKBs, newEntityValidator,
//        lexicalVariationEngine, useLemmasForMatching)
  }
}
