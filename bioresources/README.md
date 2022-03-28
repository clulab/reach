[![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.clulab/bioresources/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.clulab/bioresources)

# bioresources
Data resources from the biomedical domain

## Information for developers

### Extending grounding resource files
The `src/main/resources/org/clulab/reach/kb` folder contains a number of
tab separated value (TSV) files which contain grounding entries. Several of
these files have corresponding automated update scripts in the `scripts`
folder. If an update script exists, the corresponding TSV file should not be
manually edited, rather, changes should be integrated by changing and running
the script.

Most TSV files contain primary grounding entries from a given source.
Additionally, the `NER-Grounding-Override.tsv` file contains manually curated
groundings that are used to apply overrides.

If the goal is to use entries from a new TSV file for named entity recognition
and grounding, a new entry needs to be added to
`src/main/resources/application.conf` under the `KnowledgeBases` block.
Additonally, if a TSV file introduces a new entity type that isn't yet used
in Reach, this needs to be added to the taxonomy (main/src/main/resources/org/clulab/reach/biogrammar/taxonomy.yml),
in an appropriate position, and a new rule needs to be added to
main/src/main/resources/org/clulab/reach/biogrammar/entities/entities.yml to
recognize tagged entities of the given type.
