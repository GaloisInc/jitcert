# JitCert Web

## Warning

JitCert's web interface allows users to run arbitrary code. 
Do not run this on untrusted or public networks!

## Dependencies

### Haskell

Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/). 
If you have not installed the required version of `ghc`, you probably will need to run `stack setup`.

### Yesod

Install `yesod` with:

    stack install yesod-bin

### Postgresql

Install [Postgresql](https://www.postgresql.org/download/) on your machine. 
Make sure you can run `psql` (you may need to add the Postgresql binary directory to your `$PATH`).

Setup your database by running `psql` and create a user and database with:

    CREATE USER "jitcert-web" WITH PASSWORD 'jitcert-web';
    CREATE DATABASE "jitcert-web" OWNER "jitcert-web" ENCODING 'UTF8';

You may need additional configuration to enable password logins for Postgresql.

## Installation

Compile `jitcert-web` with:

    stack build

Run `jitcert-web` in development mode:

    stack exec -- yesod devel

The server should now be running at [localhost:3000](http://localhost:3000/). 
Any changes to source files (should) force the server to recompile. 

