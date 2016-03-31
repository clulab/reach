[![Build Status](https://travis-ci.org/clulab/reach.svg?branch=master)](https://travis-ci.org/clulab/reach)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.clulab/reach_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.clulab/reach_2.11)

Reach
=====

# What is it?

Reach stands for **Re**ading and **A**ssembling **C**ontextual and **H**olistic Mechanisms from Text. In plain English, Reach is an information extraction system for the biomedical domain, which aims to read scientific literature and extract cancer signaling pathways. Reach implements a fairly complete extraction pipeline, including: recognition of biochemical entities (proteins, chemicals, etc.), grounding them to known knowledge bases such as Uniprot, extraction of BioPAX-like interactions, e.g., phosphorylation, complex assembly, positive/negative regulations, and coreference resolution, for both entities and interactions.  

Reach is developed using [Odin](https://github.com/clulab/processors/wiki/ODIN-(Open-Domain-INformer)), our open-domain information extraction framework, which is released within our [`processors`](https://github.com/clulab/processors) repository. 

Please scroll down to the bottom of this page for additional resources, including a Reach output visualizer, REST API, and datasets created with Reach.

# Licensing
All our own code is licensed under Apache License Version 2.0. **However, some of the libraries used here, most notably CoreNLP, are GPL v2.** If `BioNLPProcessor` is not removed from this package, technically our whole code becomes GPL v2 since `BioNLPProcessor` builds on Stanford's `CoreNLP` functionality. Soon, we will split the code into multiple components, so licensing becomes less ambiguous.

# Changes
+ **1.2.2** - Added cellular locations to context. Context engine now can be configured by the user. Added a new deterministic context policy that extends context before and after a mention. Added support for Pandas output format. Bug fixes: context is now enabled by default; coref engine now matches the Hobbs antecedent search heuristic. 
+ **1.2.1** - Bug fix in the unboxing of controller events. New Year's Eve release!
+ **1.2.0** - First release of context extraction! Context includes: species, organs, cell lines and types. Improved coreference resolution with constraints on determiner type.
+ [more...](CHANGES.md)

# Authors  

Reach was created by the following members of the [CLU lab at the University of Arizona](http://clulab.cs.arizona.edu/):

+ [Marco Valenzuela](https://github.com/marcovzla)  
+ [Gus Hahn-Powell](https://github.com/myedibleenso)  
+ [Dane Bell](https://github.com/danebell)  
+ [Tom Hicks](https://github.com/hickst)  
+ [Enrique Noriega](https://github.com/enoriega)  
+ [Mihai Surdeanu](https://github.com/MihaiSurdeanu)  

# Citations

If you use Reach, please cite this paper:

```
@inproceedings{Valenzuela+:2015aa,
  author    = {Valenzuela-Esc\'{a}rcega, Marco A. and Gustave Hahn-Powell and Thomas Hicks and Mihai Surdeanu},
  title     = {A Domain-independent Rule-based Framework for Event Extraction},
  organization = {ACL-IJCNLP 2015},
  booktitle = {Proceedings of the 53rd Annual Meeting of the Association for Computational Linguistics and the 7th International Joint Conference on Natural Language Processing of the Asian Federation of Natural Language Processing: Software Demonstrations (ACL-IJCNLP)},
  url = {http://www.aclweb.org/anthology/P/P15/P15-4022.pdf},
  year      = {2015},
  pages = {127--132},
  Note = {Paper available at \url{http://www.aclweb.org/anthology/P/P15/P15-4022.pdf}},
}
```

More publications from the Reach project are available [here](https://github.com/clulab/reach/wiki/Publications).

# Installation

This software requires Java 1.8, Scala 2.11, and CoreNLP 3.x or higher.

The `jar` is available on Maven Central. To use, simply add the following dependency to your `pom.xml`:

```xml
<dependency>
   <groupId>org.clulab</groupId>
   <artifactId>reach_2.11</artifactId>
   <version>1.2.2</version>
</dependency>
```

 The equivalent SBT dependencies are:

```scala
libraryDependencies ++= Seq(
    "org.clulab" %% "reach" % "1.2.2"
)
```

# How to compile the source code

This is a standard sbt project, so use the usual commands (i.e. `sbt compile`, `sbt assembly`, etc) to compile.
Add the generated jar files under `target/` to your `$CLASSPATH`, along with the other necessary dependency jars. Take a look at `build.sbt` to see which dependencies are necessary at runtime.

# How to use it

## Running the system on a directory of `.nxml` papers

The most common usage of Reach is to parse a directory containing one or more papers in the NXML format.
In order to run the system on such a directory of papers, you must create a `.conf` file.  See `src/main/resources/application.conf` for an example configuration file.  The directory containing NXML files should be specified using the `nxmlDir` variable.

```scala
sbt "runMain edu.arizona.sista.reach.ReachCLI /path/to/yourapplication.conf"
```

If the configuration file is omitted, Reach uses the default `.conf`. That is, the command:

```scala
sbt "runMain edu.arizona.sista.reach.ReachCLI"
```

will run the system using the `.conf` file under `src/main/resources/application.conf`.

## Running the interactive Reach shell for rule debugging

```scala
sbt "runMain edu.arizona.sista.reach.ReachShell"
```

enter `:help` to get a list of available commands.

## Running the sieve-based assembly system
Reach now provides a sieve-based system for assembly of event mentions.  While still under development, the system currently has support for (1) exact deduplication for both entity and event mentions, (2) unification of mentions through coreference resolution, and (3) the reporting of intra-sentence causal precedence relations (ex. A causally precedes B) using linguistics features.  Future versions will include additional sieves for inter-sentence precedence and improved approximate deduplication.  The sieve assembly system can be run over... 

1. a directory of `.nxml` and/or `.csv` files   
 ```scala
 sbt "runMain edu.arizona.sista.assembly.RunAssembly"
 ```
2. a dataset comprised of serialized mentions  
 ```scala
 sbt "runMain edu.arizona.sista.assembly.AssembleFromDataset"
 ```
 
Be sure to specify the input source and output directory for assembly in the [`application.conf`](https://github.com/clulab/reach/blob/ea0b42c23a70d7dda1bc01b62bfc716129d24d9f/src/main/resources/application.conf#L63-L80).

Currently, two `.csv` files are produced for assembly results **within** each paper:  

1. results meeting [MITRE's (March 2016) requirements](https://github.com/clulab/reach/blob/3d4f82c87f1b4c7299ff2ceae8adc352212bd430/src/main/scala/edu/arizona/sista/assembly/AssemblyExporter.scala#L337-L352)
2. results without MITRE's constraints

Two additional output files are produced for assembly results **across** all papers:  

1. results meeting [MITRE's (March 2016) requirements](https://github.com/clulab/reach/blob/3d4f82c87f1b4c7299ff2ceae8adc352212bd430/src/main/scala/edu/arizona/sista/assembly/AssemblyExporter.scala#L337-L352)  
2. results without MITRE's constraints

# Modifying the code
Reach builds upon our Odin event extraction framework. If you want to modify event and entity grammars, please refer to [Odin's Wiki](https://github.com/sistanlp/processors/wiki/ODIN-(Open-Domain-INformer)) page for details. Please read the included Odin manual for details on the rule language and the Odin API.

# Reach web services

We have developed a series of web services on top of the Reach library. All are freely available [here](http://agathon.sista.arizona.edu:8080/odinweb/).

# Reach datasets

We have generated multiple datasets by reading publications from the [open-access PubMed subset](http://www.ncbi.nlm.nih.gov/pmc/tools/openftlist/) using Reach. All datasets are freely available [here](https://github.com/clulab/reach/wiki/Datasets).

# Funding

The development of Reach was funded by the [DARPA Big Mechanism program](http://www.darpa.mil/program/big-mechanism) under ARO contract W911NF-14-1-0395.
