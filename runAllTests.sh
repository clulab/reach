#!/usr/bin/env bash

# uncomment the lines below if processorAnnotator.type = "server"
# start up the Processors Server for tests to run against:
#echo 'Starting Processor Server...'
#sbt 'run-main org.clulab.processors.server.ProcessorServer' &
#sleep 20

overallExitCode=0

sbt test

if [ $? -ne 0 ]; then
    echo "The test suite failed!"
    overallExitCode=1
fi

sbt main/test

if [ $? -ne 0 ]; then
    echo "The main/test suite failed!"
    overallExitCode=1
fi

sbt causalAssembly/test

if [ $? -ne 0 ]; then
    echo "The causalAssembly/test suite failed!"
    overallExitCode=1
fi

sbt export/test

if [ $? -ne 0 ]; then
    echo "The export/test suite failed!"
    overallExitCode=1
fi

#echo 'Stopping Processor Server...'
#sbt 'run-main org.clulab.processors.csshare.ShutdownProcessorServer'

if [ $overallExitCode -ne 0 ]; then
	echo "At least one test suite failed!"
	exit 1
fi