FROM ubuntu:focal

# Install basic packages and Java
RUN apt-get update
RUN DEBIAN_FRONTEND="noninteractive" apt-get install -y git curl \
    vim wget build-essential software-properties-common openjdk-8-jdk

# Install Scala
RUN wget -nv https://downloads.lightbend.com/scala/2.12.8/scala-2.12.8.deb && \
    dpkg -i scala-2.12.8.deb

# Install SBT
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list && \
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add && \
    apt-get update && \
    apt-get install sbt=1.4.0

# Clone and pre-build Reach so that the server starts up fast upon launch
RUN git clone https://github.com/clulab/reach.git && \
    cd reach && \
    sed -i 's/host = localhost/host = 0.0.0.0/' export/src/main/resources/reference.conf && \
    sbt publishLocal

WORKDIR reach
ENTRYPOINT ["sbt", "runMain org.clulab.reach.export.server.ApiServer"]
