#!/bin/sh
#  Run all tests related to Grounding.
#
sbt 'test-only edu.arizona.sista.reach.TestKBSupport \
edu.arizona.sista.reach.TestAzFailsafeKB.scala \
edu.arizona.sista.reach.TestTsvKBs \
edu.arizona.sista.reach.TestAdHocIMKBs \
edu.arizona.sista.reach.TestProteinResolutions \
edu.arizona.sista.reach.TestFamilyResolutions \
edu.arizona.sista.reach.TestOrganCellTypeResolutions \
edu.arizona.sista.reach.TestGrounding'
