#!/bin/sh -ex
mirage configure config_server.ml --xen --no-opam
make
mirage configure config_client.ml --xen --no-opam
make
