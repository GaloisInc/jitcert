# JitCert

JitCert is a prototype domain specific language (DSL) for defining, rendering, checking, and querying assurance case arguments in Goal Structuring Notation (GSN). 
Its main features include setting values for objects and evidence in an argument, tracking dependencies between different pieces of an argument, automatically identifying changes in an argument, and logically evaluating a GSN argument. 
JitCert is implemented as an embedded DSL in Haskell.

JitCert is implemented in two packages. Core functionality for building, checking, and querying GSNs is in the [jitcert-core](./jitcert-core/) library. 
A web interface is provided in [jitcert-web](./jitcert-web/) to display and query GSNs in the browser. 

# Warning

JitCert's web interface (in [jitcert-web](./jitcert-web/)) allows users to run arbitrary code. 
This is intended for local development only and runs any code sent to it. 
*Do not* run this on untrusted or public networks!

# Quick start

With [Docker](docker.com) installed, run `docker-compose build` then `docker-compose up` from the directory contaning the Dockerfile. Finally, navigate to `localhost:3000` in your browser to see the JitCert web interface.
Note, it may be necessary to increase your default RAM setting in Docker to complete the build. 

# Maintenance

We currently do not have any plans to continue maintaining and updating this project. 
