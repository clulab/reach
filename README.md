[![Build Status](http://jenkins.cs.arizona.edu:8090/buildStatus/icon?job=reach%2Fmaster)](http://jenkins.cs.arizona.edu:8090/job/reach)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.clulab/reach_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.clulab/reach_2.11)

Reach
=====

# What is it?

Reach stands for **Re**ading and **A**ssembling **C**ontextual and **H**olistic Mechanisms from Text. In plain English, Reach is an information extraction system for the biomedical domain, which aims to read scientific literature and extract cancer signaling pathways. Reach implements a fairly complete extraction pipeline, including: recognition of biochemical entities (proteins, chemicals, etc.), grounding them to known knowledge bases such as Uniprot, extraction of BioPAX-like interactions, e.g., phosphorylation, complex assembly, positive/negative regulations, and coreference resolution, for both entities and interactions.

Reach is developed using [Odin](https://github.com/clulab/processors/wiki/ODIN-(Open-Domain-INformer)), our open-domain information extraction framework, which is released within our [`processors`](https://github.com/clulab/processors) repository.

Please scroll down to the bottom of this page for additional resources, including a Reach output visualizer, REST API, and datasets created with Reach.

# Licensing
This project is, and will always be, free for *research purposes*. However, starting with version 1.2, we are using a license that restricts its use for *commercial purposes*. Please contact us for details.

# Changes
+ **1.4.0** - Require separate Processors Server (Reach is the client). Add CLUProcessor type.
+ **1.4.0** - Update to use Processors 6.1.5 (for refactored Client/Server). Update run scripts.
+ [much more...](CHANGES.md)

# Authors

Reach was created by the following members of the [CLU lab at the University of Arizona](http://clulab.cs.arizona.edu/):

+ [Marco Valenzuela](https://github.com/marcovzla)
+ [Gus Hahn-Powell](https://github.com/myedibleenso)
+ [Dane Bell](https://github.com/danebell)
+ [Tom Hicks](https://github.com/hickst)
+ [Enrique Noriega](https://github.com/enoriega)
+ [Mihai Surdeanu](https://github.com/MihaiSurdeanu)
+ [Clayton Morrison](https://ischool.arizona.edu/users/clayton-morrison)

# Citations

If you use Reach, please cite one of the following papers:

```
@Article{Escarcega:2018,
  author={Valenzuela-Esc{\'a}rcega, Marco A and Babur, {\"O}zg{\"u}n and Hahn-Powell, Gus and Bell, Dane and Hicks, Thomas and Noriega-Atala, Enrique and Wang, Xia and Surdeanu, Mihai and Demir, Emek and Morrison, Clayton T},
  title={Large-scale Automated Machine Reading Discovers New Cancer Driving Mechanisms},
  journal={Database: The Journal of Biological Databases and Curation},
  url={http://clulab.cs.arizona.edu/papers/escarcega2018.pdf},
  doi={10.1093/database/bay098},
  year={2018},
  publisher={Oxford University Press}
}
```

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
   <version>1.4.0</version>
</dependency>
```

 The equivalent SBT dependencies are:

```scala
libraryDependencies ++= Seq(
    "org.clulab" %% "reach" % "1.4.0"
)
```

# How to compile the source code

This is a standard sbt project, so use the usual commands (i.e. `sbt compile`, `sbt assembly`, etc.) to compile.
Add the generated jar files under `target/` to your `$CLASSPATH`, along with the other necessary dependency jars. Take a look at `build.sbt` to see which dependencies are necessary at runtime.

# Running Reach

## Processing a directory of `.nxml` papers

The most common use of Reach is to process a directory containing one or more papers in the proper formats. A Wiki page documents the [supported input formats](https://github.com/clulab/reach/wiki/Supported-Input-Formats).

Some configuration is necessary before running Reach. Please refer to the [Running Reach](https://github.com/clulab/reach/wiki/Running-Reach) Wiki page for detailed information on configuring and running Reach.

## The Interactive Shell

An interactive shell can be run from the command line to process small fragments of entered text. The shell is useful for reviewing and understanding the operation of Reach, including NER, entity, and event processing and rule debugging. To start a Reach shell, run the [runReachShell.sh](https://github.com/clulab/reach/blob/master/runReachShell.sh) script:

```
runReachShell.sh
```

At the shell prompt enter `:help` to get a list of available commands.

# Modifying the code
Reach builds upon our Odin event extraction framework. If you want to modify event and entity grammars, please refer to [Odin's Wiki](https://github.com/sistanlp/processors/wiki/ODIN-(Open-Domain-INformer)) page for details. Please read the included Odin manual for details on the rule language and the Odin API.

# Reach web services

We have developed a series of web services on top of the Reach library. All are freely available [here](http://agathon.sista.arizona.edu:8080/odinweb/).

# Reach datasets

We have generated multiple datasets by reading publications from the [open-access PubMed subset](http://www.ncbi.nlm.nih.gov/pmc/tools/openftlist/) using Reach. All datasets are freely available [here](https://github.com/clulab/reach/wiki/Datasets).

# Funding

The development of Reach was funded by the [DARPA Big Mechanism program](http://www.darpa.mil/program/big-mechanism) under ARO contract W911NF-14-1-0395.
