[![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.clulab/bioresources/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.clulab/bioresources)
FOURTHLINE
THIRD LINE
ANOTHER TEST LINE
TEST LINE BY ENRIQUE

# bioresources
Data resources from the biomedical domain

## Information for developers

### Extending grounding resource files
The `src/main/resources/org/clulab/reach/kb` folder contains a number of
tab separated value (TSV) files which contain grounding entries. Several of
these files have corresponding automated update scripts in the `scripts`
folder.  If an update script exists, the corresponding TSV file should not be
manually edited, rather, changes should be integrated by changing and running
the script.

Most TSV files contain primary grounding entries from a given source.
Additionally, the `NER-Grounding-Override.tsv` file contains manually curated
groundings that are used to apply overrides.

Note that the files are version-controlled in a gzipped form and
therefore need to be decompressed for editing and then compressed again
for checking in to version control.

Note also that if a new TSV file or a new entity type needs to be added. it
requires corresponding changes in several places in the Reach code base. For an
example of changes that needed to be made in Reach when adding
`mesh_disease.tsv` to bioresources, see
https://github.com/clulab/reach/pull/686/files.

### Updating the NER files
Once edits have been made to one or more files in the `kb` folder, the NER
files need to be regenerated. For this, `reach` needs to be cloned in the same
parent folder in which the `bioresources` repo was cloned. Then, the
[`ner_kb.sh`](ner_kb.sh) script needs to be run which converts the KBs in
`org.clulab.reach.kb` into the format expected by the BioNLPProcessor NER.
Please re-run this script everytime a grounding file changes, or when the
tokenization algorithm changes in BioNLPProcessor.

The `ner_kb.sh` script uses [`ner_kb.config`](ner_kb.config) as a configuration
input. If only a small number of KBs were modified, edit the file and keep
only the modified KBs to avoid re-generating *all* KBs. The config file
also controls what organisms' gene/protein synonyms should be included in the
NER resources. By default, only human proteins are included but additional
organism names can be listed in `ner_kb.config` to extend NER to these
organisms.

### Testing bioresources updated with Reach
To test changes in bioresources, first, bioresources need to be built using
`sbt` as follows:
```
sbt publishLocal
```
then check `version.sbt` to get the current published version of bioresources,
typically something like `x.x.x-SNAPSHOT`. Then navigate to the reach repo,
edit `processors/build.sbt` and change the bioresources version to the one
published. This will result in Reach using the locally published bioresources.

It is also possible to automatically build a custom branch of bioresources
and Reach, and then run Reach tests using Docker. This is documented here:
[`https://github.com/clulab/reach/tree/master/docker`](https://github.com/clulab/reach/tree/master/docker).
