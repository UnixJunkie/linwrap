#!/bin/bash

gunzip -f -k data/training.liblinear.gz

make

_build/default/src/linwrap.exe -np 16 --scan-C -i data/training.liblinear 2>&1 \
    | grep tstAUC
