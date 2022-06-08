#!/usr/bin/env bash

# uncomment the lines below if processorAnnotator.type = "server"
#echo 'Starting Processor Server...'
#sbt 'run-main org.clulab.processors.server.ProcessorServer' &
#sleep 20

echo 'Running Reach CLI...'
sbt 'runMain org.clulab.reach.RunReachCLI'

# An explicit call to shutdown the server is not necessary
# because ReachCLI shuts down both client and server.
