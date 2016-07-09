#!/bin/sh
#  Run all tests related to Grounding.
#
sbt 'test-only \
org.clulab.reach.TestAdHocIMKBs \
org.clulab.reach.TestAzFailsafeKB.scala \
org.clulab.reach.TestFamilyResolutions \
org.clulab.reach.TestGrounding \
org.clulab.reach.TestGroundingTrait \
org.clulab.reach.TestKBSupport \
org.clulab.reach.TestModelEntities.scala \
org.clulab.reach.TestNERLabeling.scala \
org.clulab.reach.TestOrganCellTypeResolutions \
org.clulab.reach.TestProteinResolutions \
org.clulab.reach.TestReachContextKBLister.scala \
org.clulab.reach.TestReachGrounder.scala \
org.clulab.reach.TestTsvKBs'
