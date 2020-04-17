FROM ubuntu:latest

# Install basic packages
RUN apt-get update && apt-get install -y git vim wget build-essential \
    software-properties-common

# Add some extensions to apt sources and update
RUN add-apt-repository -y ppa:openjdk-r/ppa && \
    echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list && \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 && \
    apt-get update

# Install Java, Scala and SBT
RUN apt-get install -y openjdk-8-jdk && \
    wget -nv https://www.scala-lang.org/files/archive/scala-2.11.12.deb && \
    dpkg -i scala-2.11.12.deb && \
    rm scala-2.11.12.deb && \
    apt-get install -y sbt

# Clone and pre-build Reach so that the server starts up fast upon launch
RUN git clone https://github.com/clulab/reach.git && \
    cd reach && \
    sed -i 's/host = localhost/host = 0.0.0.0/' export/src/main/resources/reference.conf && \
    sbt publishLocal

WORKDIR reach
ENTRYPOINT ["sbt", "run-main org.clulab.reach.export.server.ApiServer"]
