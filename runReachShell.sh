#!/usr/bin/env bash

echo 'Starting Processor Server...'
sbt 'run-main org.clulab.processors.server.ProcessorServer' >/dev/null 2>&1 &
sleep 20

echo 'Starting Reach Shell...'
sbt 'run-main org.clulab.reach.ReachShell'

echo 'Stopping Processor Server...'
sbt 'run-main org.clulab.processors.csshare.ShutdownProcessorServer'
