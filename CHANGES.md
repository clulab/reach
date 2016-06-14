#Changes
=======
+ **1.3.0** - Use Bioresources 1.1.9, Processors 5.8.5. Allow regulations of regulations. Identify X inhibitors as chemicals. Use MITRE model for NER/grounding. Add NER stop list. Add Translocation mention support to Assembly. Various rule fixes and enhancements. Allow PaperReader to read .csv files.
+ **1.2.3** - Update to use Bioresources version 1.1.8, which includes NER stop list.
+ **1.2.3** - Update code and tests to replace homemade KBs, in conjunction with Bioresources version 1.1.7.
+ **1.2.3** - Sieve-based assembly system that includes a feature-based classifier and rule-based sieves for detecting causal precedence.
+ **1.2.3** - Bug fix and optimizations to the `AssemblyManager`.
+ **1.2.3** - Updated to Bioresources 1.1.6, Processors 5.8.4. Add/use Uniprot Tissue type KB.
+ **1.2.3** - Add new context frames and pointers to FRIES output event stream.
+ **1.2.3** - Replace ChEBI and HMDB KBs with PubChem KB.
+ **1.2.3** - Add isDirect flag to BioEventMentions to track if a regulation is direct or not.
+ **1.2.3** - Add "reverse" PTM events (e.g., dephosphorylation, deubiquitination, etc).
+ **1.2.3** - User PFAM protein family KB for NER and grounding.
+ **1.2.3** - Context is assigned to all type of mentions, not just event mentions.
+ **1.2.3** - Improvements to event extraction grammars.
+ **1.2.2** - Added cellular locations to context. Context engine now can be configured by the user. Added a new deterministic context policy that extends context before and after a mention. Added support for Pandas output format. Bug fixes: context is now enabled by default; coref engine now matches the Hobbs antecedent search heuristic.
+ **1.2.1** - Bug fix in the unboxing of controller events. New Year's Eve release!
+ **1.2.0** - First release of context extraction! Context includes: species, organs, cell lines and types. Improved coreference resolution with constraints on determiner type.
+ **1.1.3** - Bug fixes in coreference resolution plus output formats
+ **1.1.2** - Bug fixes in the index card output
+ **1.1.1** - Improved event coreference resolution
+ **1.1.0** - Added entity and event coreference resolution
+ **1.0.0** - Initial release
