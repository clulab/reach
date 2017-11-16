#!/usr/bin/env bash

# start up the Processors Server for tests to run against:
echo 'Starting Processor Server...'
sbt 'run-main org.clulab.processors.server.ProcessorServer' &
sleep 20

sbt test
sbt main/test
sbt causalAssembly/test
sbt export/test

echo 'Stopping Processor Server...'
sbt 'run-main org.clulab.processors.csshare.ShutdownProcessorServer'
