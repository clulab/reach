`reach-assembly`
=====

# What is it?

`reach-assembly` is the assembly arm of [`reach`](https://github.com/clulab/reach). This project provides a sieve-based system for assembly of event mentions.  While still under development, the system currently has support for (1) exact deduplication for both entity and event mentions, (2) unification of mentions through coreference resolution, and (3) the reporting of intra and inter-sentence causal precedence relations (ex. A causally precedes B) using linguistic features, and (4) a feature-based classifier for causal precedence.  Future versions will include additional sieves for causal precedence and improved approximate deduplication.

For more details on the sieve-based assembly system, please refer to the following paper:

```
@inproceedings{GHP+:2016aa,
  author       = {Gus Hahn-Powell and Dane Bell and Marco A. Valenzuela-Esc\'{a}rcega and Mihai Surdeanu},
  title        = {This before That: Causal Precedence in the Biomedical Domain},
  booktitle    = {Proceedings of the 2016 Workshop on Biomedical Natural Language Processing},
  organization = {Association for Computational Linguistics}
  year         = {2016}
  Note         = {Paper available at \url{https://arxiv.org/abs/1606.08089}}
}
```

# Changes
+ **0.0.1** - Assembly system from `reach` v1.3.2
+ [more...](CHANGES.md)

# Authors  

The assembly system was created by the following members of the [CLU lab at the University of Arizona](http://clulab.cs.arizona.edu/):

+ [Gus Hahn-Powell](https://github.com/myedibleenso)  
+ [Dane Bell](https://github.com/danebell)  
+ [Marco Valenzuela](https://github.com/marcovzla)  
+ [Mihai Surdeanu](https://github.com/MihaiSurdeanu)

# Citations

If you use `reach-assembly`, please cite this paper:

```
@inproceedings{GHP+:2016aa,
  author       = {Gus Hahn-Powell and Dane Bell and Marco A. Valenzuela-Esc\'{a}rcega and Mihai Surdeanu},
  title        = {This before That: Causal Precedence in the Biomedical Domain},
  booktitle    = {Proceedings of the 2016 Workshop on Biomedical Natural Language Processing},
  organization = {Association for Computational Linguistics}
  year         = {2016}
  Note         = {Paper available at \url{https://arxiv.org/abs/1606.08089}}
}
```

More publications from the Reach project are available [here](https://github.com/clulab/reach/wiki/Publications).

# Including `reach-assembly` in your code

This software requires Java 1.8.

The `jar` is available on Maven Central. To use, simply add the following dependency to your `build.sbt` (NOTE: replace yourVersion with the version you wish to use):

```scala
libraryDependencies ++= Seq(
    "org.clulab" %% "reach-assembly" % "yourVersion"
)
```

# How to compile the source code

This is a standard `sbt` project, so use the usual commands (i.e. `sbt compile`, `sbt assembly`, etc.) to compile.
Add the generated jar files under `target/` to your `$CLASSPATH`, along with the other necessary dependency jars. Take a look at `build.sbt` to see which dependencies are necessary at runtime.

# Running things

### The interactive Assembly shell

You can run interactively explore assembly output for various snippets of text using the assembly shell:

```scala
sbt "runMain org.clulab.reach.assembly.AssemblyShell"
```

# Modifying the code

Reach builds upon our Odin event extraction framework. If you want to modify event and entity grammars, please refer to [Odin's Wiki](https://github.com/sistanlp/processors/wiki/ODIN-(Open-Domain-INformer)) page for details. Please read the included Odin manual for details on the rule language and the Odin API.

# Funding

The development of Reach was funded by the [DARPA Big Mechanism program](http://www.darpa.mil/program/big-mechanism) under ARO contract W911NF-14-1-0395.
