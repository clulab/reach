#!/bin/sh
#  Run all tests related to Grounding.
#
sbt 'test-only org.clulab.reach.TestKBSupport \
org.clulab.reach.TestAzFailsafeKB.scala \
org.clulab.reach.TestTsvKBs \
org.clulab.reach.TestAdHocIMKBs \
org.clulab.reach.TestGroundingTrait \
org.clulab.reach.TestProteinResolutions \
org.clulab.reach.TestFamilyResolutions \
org.clulab.reach.TestOrganCellTypeResolutions \
org.clulab.reach.TestReachContextKBLister.scala \
org.clulab.reach.TestGrounding'
