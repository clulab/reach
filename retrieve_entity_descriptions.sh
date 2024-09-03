#!/bin/bash
# Prints the synonyms of each entity type located in the KB files

IFS=':'

for ARG in "$@"
do
  read -ra ID <<< "$ARG"
  grep -r "${ID[1]}"  bioresources/src/main/resources/org/clulab/reach/kb/ | \
    awk -F: '{print $2}' | \
    awk -F'\t' '{printf("\"%s\",\n", tolower($1))}' | \
    uniq
done