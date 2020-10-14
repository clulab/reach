package org.clulab.reach

import org.scalatest.Suites

class PolaritySuite extends Suites(
  new TestRegulationEvents,
  new TestActivationEvents,
  new TestPolarity,
  new TestHyphenedEvents,
  new NegationTests,
  new HypothesisTests,
  new TestTemplaticAutoEvents,
  new TestTemplaticSimpleDeEvents,
  new TestTemplaticSimpleEvents,
  new TestTranscriptionEvents,
  new TestTranslocationEvents
)
