#Changes
=======

+ **1.3.3** - Find shortest path between heads in actions. Add/allow outputter for JSON serialization format.
+ **1.3.3** - Fix trigger affecting countSemanticNegatives. Add restart capability and more logging stats to ReachCLI.
+ **1.3.3** - Added a new tabular format for the DyCE CMU model. Update Phase3 use cases. Add/use root path in config.
+ **1.3.3** - The "arizona" tabular format was extended with source/destination for Translocation events.
+ **1.3.3** - Use Processors 6.0.1 and Bioresources 1.1.19, which contains Harvard Bioentities project KBs. Update many tests for BE KBs.
+ **1.3.3** - Mentions are created from previously unrecognized aliases.
+ **1.3.3** - Sub-project split into main, assembly, export.
+ **1.3.2** - Optimizations to `json` serialization/deserialization of `CorefMention`.
+ **1.3.2** - Uses bioresources 1.1.15 and processors 5.9.6.  Introduces [`json` serialization/deserialization of `CorefMention` (including grounding, modifications, etc.)](https://gist.github.com/myedibleenso/8383af789b37ba598ff64ddd12c8b35b).
+ **1.3.2** - Better handling of nested events.
+ **1.3.2** - Update to use Bioresources 1.1.15 and Processors 5.9.5.
+ **1.3.1** - Update of all edu.arizona.sista packages to org.clulab
+ **1.3.1** - Updated rules for parsing changes. Add rules for "VBN with" regulations. Correct amino acid rule. Add syntax -> surface rule. Several changes to activation/regulations rules.
+ **1.3.1** - Refactor protein domain table. Add protein kinase lookup table.
+ **1.3.1** - Fix bugs in grounding. Add ID overrides from collaborators. Add tests.
+ **1.3.1** - Replace use of manual KB files with NER/grounding override file. Add tests.
+ **1.3.1** - Refactor grounding tables to a standardized 2-5 column format. Update KB tests.
+ **1.3.0** - Use Bioresources 1.1.9, Processors 5.8.5. Allow regulations of regulations. Identify X inhibitors as chemicals. Use MITRE model for NER/grounding. Add NER stop list. Add Translocation mention support to Assembly. Various rule fixes and enhancements. Allow PaperReader to read .csv files.
+ **1.3.0** - Update to use Bioresources version 1.1.8, which includes NER stop list.
+ **1.3.0** - Update code and tests to replace homemade KBs, in conjunction with Bioresources version 1.1.7.
+ **1.3.0** - Sieve-based assembly system that includes a feature-based classifier and rule-based sieves for detecting causal precedence.
+ **1.3.0** - Bug fix and optimizations to the `AssemblyManager`.
+ **1.3.0** - Updated to Bioresources 1.1.6, Processors 5.8.4. Add/use Uniprot Tissue type KB.
+ **1.3.0** - Add new context frames and pointers to FRIES output event stream.
+ **1.3.0** - Replace ChEBI and HMDB KBs with PubChem KB.
+ **1.3.0** - Add isDirect flag to BioEventMentions to track if a regulation is direct or not.
+ **1.3.0** - Add "reverse" PTM events (e.g., dephosphorylation, deubiquitination, etc).
+ **1.3.0** - User PFAM protein family KB for NER and grounding.
+ **1.3.0** - Context is assigned to all type of mentions, not just event mentions.
+ **1.3.0** - Improvements to event extraction grammars.
+ **1.2.2** - Added cellular locations to context. Context engine now can be configured by the user. Added a new deterministic context policy that extends context before and after a mention. Added support for Pandas output format. Bug fixes: context is now enabled by default; coref engine now matches the Hobbs antecedent search heuristic.
+ **1.2.1** - Bug fix in the unboxing of controller events. New Year's Eve release!
+ **1.2.0** - First release of context extraction! Context includes: species, organs, cell lines and types. Improved coreference resolution with constraints on determiner type.
+ **1.1.3** - Bug fixes in coreference resolution plus output formats
+ **1.1.2** - Bug fixes in the index card output
+ **1.1.1** - Improved event coreference resolution
+ **1.1.0** - Added entity and event coreference resolution
+ **1.0.0** - Initial release
