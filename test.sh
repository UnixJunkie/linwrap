#!/bin/bash

set -x # DEBUG

gunzip -f -k data/training.liblinear.gz

make

_build/default/src/linwrap.exe -q -np 16 --scan-C -i data/training.liblinear
