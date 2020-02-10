FROM openjdk:8u181-jdk
WORKDIR /workdir
RUN apt-get update && curl -sSL \
    https://get.haskellstack.org/ | sh
COPY stack.yaml .
COPY stack.yaml.lock .
COPY typelevelLR.cabal .
RUN stack setup
RUN apt-get install -y scala
ENV PATH=$PATH:/root/.local/bin
