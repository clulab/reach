#!/usr/bin/env bash

echo 'Starting Processor Server...'
sbt 'run-main org.clulab.processors.server.ProcessorServer' &
sleep 20

echo 'Running Reach CLI...'
sbt 'run-main org.clulab.reach.RunReachCLI'
