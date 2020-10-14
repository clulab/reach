#!/usr/bin/env bash

# uncomment the lines below if processorAnnotator.type = "server"
# start up the Processors Server for tests to run against:
#echo 'Starting Processor Server...'
#sbt 'runMain org.clulab.processors.server.ProcessorServer' &
#sleep 20

# Don't start sbt multiple times if it is not necessary.
# This will only fail once, but we want a yes/no answer anyway.

sbt 'root/test; processors/test; main/test; export/test'

#echo 'Stopping Processor Server...'
#sbt 'runMain org.clulab.processors.csshare.ShutdownProcessorServer'
