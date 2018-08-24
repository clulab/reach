#!/usr/bin/env bash

# uncomment the lines below if processorAnnotator.type = "server"
#echo 'Starting Processor Server...'
#sbt 'run-main org.clulab.processors.server.ProcessorServer' >/dev/null 2>&1 &
#sleep 20

echo 'Starting Reach Shell...'
sbt 'run-main org.clulab.reach.ReachShell masha.conf'

#echo 'Stopping Processor Server...'
#sbt 'run-main org.clulab.processors.csshare.ShutdownProcessorServer'
