reach
=====

# What is it?

Authors:
  * ??
  
# Licensing
All our own code is licensed under Apache License Version 2.0. **However, some of the libraries used here, most notably CoreNLP, are GPL v2.** If `BioNLPProcessor` is not removed from this package, technically our whole code becomes GPL v2 since `BioNLPProcessor` builds on Stanford's `CoreNLP` functionality. Soon, we will split the code into multiple components, so licensing becomes less ambiguous.

# Changes
+ **1.0** - ???
+ [more...](CHANGES.md)

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
       <artifactId>reach_1.0</artifactId>
       <version>1.0</version>
    </dependency>

 The equivalent SBT dependencies are:

    libraryDependencies ++= Seq(
        "clulab.org" %% "reach" % "1.0"
    )

# How to compile the source code

This is a standard sbt project, so use the usual commands (i.e. `sbt compile`, `sbt assembly`, etc) to compile.
Add the generated jar files under `target/` to your `$CLASSPATH`, along with the other necessary dependency jars. Take a look at `build.sbt` to see which dependencies are necessary at runtime.

# How to use it

## Common scenarios

# Modifying the code
## The Odin event extraction framework
If you want to modify event and entity grammars, please refer to [Odin's Wiki](https://github.com/sistanlp/processors/wiki/ODIN-(Open-Domain-INformer)) page for details.
