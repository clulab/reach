#!/bin/sh
#  Run all tests related to Grounding.
#
sbt 'test-only \
org.clulab.reach.TestKBSupport \
org.clulab.reach.TestKBLookupSet \
org.clulab.reach.TestKBKeyTransforms \
org.clulab.reach.TestIMKB \
org.clulab.reach.TestModelEntities \
org.clulab.reach.TestGroundingTrait \
org.clulab.reach.TestNERLabeling \
org.clulab.reach.TestReachGrounder \
org.clulab.reach.TestAzFailsafeKB \
org.clulab.reach.TestAdHocIMKBs \
org.clulab.reach.TestTsvKBs \
org.clulab.reach.TestOverrides \
org.clulab.reach.TestReachKBLookupSets \
org.clulab.reach.TestProteinResolutions \
org.clulab.reach.TestFamilyResolutions \
org.clulab.reach.TestComplexResolutions \
org.clulab.reach.TestOrganResolutions \
org.clulab.reach.TestReachContextKBLister \
org.clulab.reach.TestGrounding'
