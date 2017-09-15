#!/bin/bash

rm -rf semver-from-git rico
raco exe semver-from-git.rkt && raco distribute rico semver-from-git
