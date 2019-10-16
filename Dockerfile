FROM fpco/stack-build:lts-12.26 as build

USER root

ENV DEBIAN_FRONTEND noninteractive
RUN apt-add-repository -yu ppa:jonathonf/python-3.6
RUN apt install -y python3.6
RUN python3.6 -m pip install --upgrade pip

COPY ./jitcert-core/cabal.project /opt/build/jitcert-core/
COPY ./jitcert-core/stack.yaml /opt/build/jitcert-core/
COPY ./jitcert-core/JitCert.cabal /opt/build/jitcert-core/

COPY ./jitcert-web/stack.yaml /opt/build/jitcert-web/
COPY ./jitcert-web/jitcert-web.cabal /opt/build/jitcert-web/

RUN stack update
RUN cd /opt/build/jitcert-core && stack build --only-snapshot --system-ghc
RUN cd /opt/build/jitcert-web && stack build --only-snapshot --system-ghc

COPY . /opt/build

RUN python3.6 -m pip install -r /opt/build/jitcert-web/nlp/requirements.txt
RUN python3.6 -m spacy download en

RUN cd /opt/build/jitcert-web && stack build --system-ghc
EXPOSE 3000
WORKDIR /opt/build/jitcert-web
ENV YESOD_PGHOST=db
CMD ["stack", "exec", "--system-ghc", "--", "jitcert-web"]
