#Changes
=======
+ **1.1.38** - Add override from RAC1
+ **1.1.37** - Update Cell Ontology groundings and add synonyms/overrides for CD4+/CD8+ T cells
+ **1.1.36** - Add additional synonyms for SARS-CoV-2 proteins.
+ **1.1.35** - Update UniProt entries and remove single-letter synonyms.
+ **1.1.34** - No longer support the serialized model file.
+ **1.1.34** - Remove ChEBI, PubChem and MeSH entries that produce spurious groundings.
+ **1.1.34** - Update UniProt to include latest entries.
+ **1.1.33** - Added MeSH diseases.
+ **1.1.32** - UP has been updated, GO significantly extended and updated, CHEBI restored and updated, and HGNC added as a source of synonyms for human proteins in UP. Better stop words. 
+ **1.1.31** - Improved overrides and fix outdated GO IDs.
+ **1.1.30** - Update all UniProt groundings. Add synonyms for human UniProt proteins from HGNC. Remove old BE groundings and overrides, and replace them with new FamPlex groundings and overrides.
+ **1.1.29** - Rerun processors 362171 with emoticon fix.
+ **1.1.28** - Rerun ner_kb.sh with update from 1.1.28.
+ **1.1.27** - Update PubChem.
+ **1.1.26** - Added missing chemicals (Reach issue #645).
+ **1.1.25** - Added serialized LexiconNER model constructed from kb/ner knowledge bases.
+ **1.1.24** - Add Harvard BioEntities updates to families and complexes from 8/22/2017.
+ **1.1.23** - Add last two proteinogenic amino acids to NER override file.
+ **1.1.23** - Use consistent SBT version across projects. Add mitochondria to NER override KB, per issue #16.
+ **1.1.22** - Replace PhaseIII override file with version of 1/17/2017.
+ **1.1.21** - Modified PhaseIII override file derived from MITRE Korkut data.
+ **1.1.20** - Add additional cancer drugs for PhaseIII eval. Add PhaseIII override file derived from MITRE Korkut data.
+ **1.1.20** - Add additional DNA damage types to Bioprocess KB. Add Goat entries to Species KB for PhaseIII eval.
+ **1.1.20** - Add additional DyCE model entries to NER Override per Gyoji/Cheryl email 11/30/16.
+ **1.1.19** - Remove NER Override conflicts with new BE KBs, add entries giving BE families priority over GGP.
+ **1.1.19** - Add 7 HMDB overrides per NMZ request of 11/23/2016. Remap previous Override complexes as synonyms to new BE complex entries.
+ **1.1.19** - Add protein family and complex KBs derived from Harvard Bioentites project. Add 'time' to NER stop list.
+ **1.1.18** - Add link/links to NER stoplist per MS. Add gene name affixes resource file. Added the FGF protein and family to the override KB.
+ **1.1.18** - Improved override list to match the CMU model. Fix last synonym truncated bug for Cellosaurus.
+ **1.1.17** - Updated the NER stop word list. Added a new KB with some cell lines from the ATCC catalog. Added a file for Reach with information about cell lines: organism, disease, and organ. Added small drug names KB. Add NER overrides from Hans error feedback analysis. Resolve and move some chemicals from biopax to override KB.
+ **1.1.16** - Lost in space while releasing to Maven Central.
+ **1.1.15** - Continue updating NER override and BioProcess KBs.
+ **1.1.14** - Update NER override and BioProcess KBs from DR3 and collaborator feedback. Update build to use Scala 2.11.8 and sbt release plugin.
+ **1.1.13** - Update protein family mapping table with PFAM namespace. Continue refining protein family mapping and NER override KBs with collaborator feedback to support Reach project.
+ **1.1.12** - Update NER/grounding override file for internal conflict. Update protein family mapping table with uniprot namespace.
+ **1.1.11** - Add NER/grounding override file and remove manual KBs. Reformat all KB files. Add protein kinase file. Update for processors package changes.
+ **1.1.10** - Add feedback entries from BG/MITRE/SRI to manual chemical and manual family KBs. Update NMZ auxliary KB.
+ **1.1.9** - Repurpose manual files: update manual GPP & family with MITRE model data.
+ **1.1.8** - Added a list of tokens that should not be marked as named entities when in lower case
+ **1.1.7** - Replace local cell-type KB with Cell Ontology, local cell line KB with Cellosaurus, local organ KB with Uberon.
+ **1.1.6** - Activate Uniprot Tissue type KB, ordered before Organs. Order Proteins before Protein Families.
+ **1.1.5** - Replace ChEBI and HMDB with PubChem subset.
+ **1.1.4** - Added PFAM protein family KB, prioritize/use in NER.
+ **1.1.3** - Added 3 new cancer terms to bio process KB. The Cell_Lines KB was renamed to CellLine.
+ **1.1.2** - Added plural forms for the celullar location KB. Removed some entries (Mum) from the Species KB, because they were introducing too many false positives.
+ **1.1.1** - Reduced species KB to most common species names. Removed tissue-type KB from the NER.
+ **1.1.0** - Added KBs in the format expected by the BioNLPProcessor NER.
+ **1.0.0** - Initial release. Copied KB resources from Reach 1.2.3-SNAPSHOT. These are used both by reach and processors (for BioNLPProcessor).
