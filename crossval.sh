#!/bin/sh

# Cross validation script
CONFIG_DIR=$1
PAPERS=$2
OUTPUT=$3


for FILE in $(ls $CONFIG_DIR); do
  #statements
  echo "Running cross val with $FILE"
  sbt "project main" "runMain org.clulab.context.ml.CrossValidation $PAPERS $CONFIG_DIR/$FILE $OUTPUT" 2> /dev/null > $FILE.log &
  sleep 10
done
