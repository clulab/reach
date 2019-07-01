#!/usr/bin/env bash

sbt "testOnly org.clulab.polarity.ml.TestRegulationEvents"
sbt "testOnly org.clulab.polarity.ml.TestActivationEvents"
sbt "testOnly org.clulab.polarity.ml.TestPolarity"
sbt "testOnly org.clulab.polarity.ml.TestHyphenedEvents"
sbt "testOnly org.clulab.polarity.ml.NegationTests"
sbt "testOnly org.clulab.polarity.ml.HypothesisTests"
sbt "testOnly org.clulab.polarity.ml.TestTemplaticAutoEvents"
sbt "testOnly org.clulab.polarity.ml.TestTemplaticSimpleDeEvents"
sbt "testOnly org.clulab.polarity.ml.TestTemplaticSimpleEvents"
sbt "testOnly org.clulab.polarity.ml.TestTranscriptionEvents"
sbt "testOnly org.clulab.polarity.ml.TestTranslocationEvents"


