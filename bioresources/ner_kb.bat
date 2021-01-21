@ECHO OFF

REM
REM Converts the Reach KBs into the format expected by BioNLPProcessor
REM Re-run this script whenever a Reach KB changes
REM To avoid rugenerating *all* KBs edit the ner_kb.config file and keep only the modified KBs
REM

cd ../reach

REM generate the NER KBs here
sbt "runMain org.clulab.processors.bionlp.ner.KBGenerator ../bioresources/ner_kb.config ../bioresources/src/main/resources/org/clulab/reach/kb/ ../bioresources/src/main/resources/org/clulab/reach/kb/ner"

cd ../bioresources
