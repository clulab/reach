reach
=====

# What is it?

`reach` stands for **Re**ading and **A**ssembling **C**ontextual and **H**olistic Mechanisms from Text. In plain English, `reach` is an information extraction system for the biomedical domain, which aims to read scientific literature and extract cancer signaling pathways. `reach` implements a fairly complete extraction pipeline, including: recognition of biochemical entities (proteins, chemicals, etc.), grounding them to known knowledge bases such as Uniprot, extraction of BioPAX-like interactions, e.g., phosphorylation, complex assembly, positive/negative regulations, and coreference resolution, for both entities and interactions.  

`reach` is developed using [Odin](https://github.com/clulab/processors/wiki/ODIN-(Open-Domain-INformer)), our open-domain information extraction framework, which is released within our [`processors`](https://github.com/clulab/processors) repository. 

Please scroll down to the bottom of this page for additional resources, including a `reach` output visualizer, REST API, and datasets created with `reach`.

# Licensing
All our own code is licensed under Apache License Version 2.0. **However, some of the libraries used here, most notably CoreNLP, are GPL v2.** If `BioNLPProcessor` is not removed from this package, technically our whole code becomes GPL v2 since `BioNLPProcessor` builds on Stanford's `CoreNLP` functionality. Soon, we will split the code into multiple components, so licensing becomes less ambiguous.

# Changes
+ **1.0.0** - Initial release 
+ [more...](CHANGES.md)

# Authors  

`reach` was created by the following members of the [`clulab` at the University of Arizona](http://clulab.cs.arizona.edu/):

+ Marco Valenzuela  
+ Gus Hahn-Powell  
+ Dane Bell  
+ Tom Hicks  
+ Enrique Noriega  
+ [Mihai Surdeanu](Mihai Surdeanu)  

# Citations

For now, please cite this paper:

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

# Installation

This software requires Java 1.8, Scala 2.11, and CoreNLP 3.x or higher.

The `jar` is available on Maven Central. To use, simply add the following dependency to your `pom.xml`:

    <dependency>
       <groupId>clulab.org</groupId>
       <artifactId>reach_2.11</artifactId>
       <version>1.0.0</version>
    </dependency>

 The equivalent SBT dependencies are:

    libraryDependencies ++= Seq(
        "clulab.org" %% "reach" % "1.0.0"
    )

# How to compile the source code

This is a standard sbt project, so use the usual commands (i.e. `sbt compile`, `sbt assembly`, etc) to compile.
Add the generated jar files under `target/` to your `$CLASSPATH`, along with the other necessary dependency jars. Take a look at `build.sbt` to see which dependencies are necessary at runtime.

# How to use it

## Running the system on a directory of `nxml` papers

The most common usage of `reach` is to parse a directory containing one or more papers in the NXML format.
In order to run the system on such a directory of papers, you must create a `.conf` file.  See `src/main/resources/application.conf` for an example configuration file.  The directory containing `nxml` files should be specified using the `nxmlDir` variable.

`sbt "runMain edu.arizona.sista.reach.ReachCLI /path/to/yourapplication.conf"`

If the configuration file is omitted, `reach` uses the default `.conf`. That is, the command:

`sbt "runMain edu.arizona.sista.reach.ReachCLI"`

will run the system using the `.conf` file under `src/main/resources/application.conf`.

## Running the interactive `reach` shell for rule debugging:

`sbt "runMain runMain edu.arizona.sista.reach.ReachShell"`

enter `:help` to get a list of available commands.

# Modifying the code
`reach` builds upon our Odin event extraction framework. If you want to modify event and entity grammars, please refer to [Odin's Wiki](https://github.com/sistanlp/processors/wiki/ODIN-(Open-Domain-INformer)) page for details. Please read the included Odin manual for details on the rule language and the Odin API.

# Additional resources

## Data
+ [Specification of the FRIES format supported by `reach`](http://de.iplantcollaborative.org/dl/d/AF93EFAA-A97D-491D-808B-257BBB1B7110/fries-data-representation-spec-3.txt)
+ [`reach` output on the 1K papers from the summer 2015 Big Mechanism DARPA evaluation](http://de.iplantcollaborative.org/dl/d/845C66EE-F84E-42BE-8E04-8D24BA6A5E5F/REACH_1kpapers_FRIESformat.tar.gz). In FRIES format, generated in June 2015.

# Funding

The development of `reach` was funded by the DARPA Big Mechanism program under ARO contract W911NF-14-1-0395.
