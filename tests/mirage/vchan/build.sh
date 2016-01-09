#!/bin/sh -ex
mirage configure -f config_server.ml --xen --no-opam
make
mirage configure -f config_client.ml --xen --no-opam
make
