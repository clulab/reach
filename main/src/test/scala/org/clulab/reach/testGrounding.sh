#!/bin/sh
#  Run all tests related to Grounding.
#
sbt 'test-only \
org.clulab.reach.TestAdHocIMKBs \
org.clulab.reach.TestAzFailsafeKB \
org.clulab.reach.TestFamilyResolutions \
org.clulab.reach.TestGrounding \
org.clulab.reach.TestGroundingTrait \
org.clulab.reach.TestKBSupport \
org.clulab.reach.TestMiscLookups \
org.clulab.reach.TestModelEntities \
org.clulab.reach.TestNERLabeling \
org.clulab.reach.TestOrganResolutions \
org.clulab.reach.TestOverrides \
org.clulab.reach.TestProteinResolutions \
org.clulab.reach.TestReachContextKBLister \
org.clulab.reach.TestReachGrounder \
org.clulab.reach.TestTsvKBs'
