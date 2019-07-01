#!/usr/bin/env bash

sbt "testOnly org.clulab.reach.TestRegulationEvents"
sbt "testOnly org.clulab.reach.TestActivationEvents"
sbt "testOnly org.clulab.reach.TestPolarity"
sbt "testOnly org.clulab.reach.TestHyphenedEvents"
sbt "testOnly org.clulab.reach.NegationTests"
sbt "testOnly org.clulab.reach.HypothesisTests"
sbt "testOnly org.clulab.reach.TestTemplaticAutoEvents"
sbt "testOnly org.clulab.reach.TestTemplaticSimpleDeEvents"
sbt "testOnly org.clulab.reach.TestTemplaticSimpleEvents"
sbt "testOnly org.clulab.reach.TestTranscriptionEvents"
sbt "testOnly org.clulab.reach.TestTranslocationEvents"


