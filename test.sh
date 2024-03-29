#!/bin/bash

#set -x # DEBUG

gunzip -f -k data/training.liblinear.gz

make

rm -f model_{01,02,05,10,20,50}.txt

NPROCS=`getconf _NPROCESSORS_ONLN`

./linwrap --no-plot -np ${NPROCS} -q -i data/training.liblinear -s model_01.txt -k 1
./linwrap --no-plot -np ${NPROCS} -q -i data/training.liblinear -s model_02.txt -k 2
./linwrap --no-plot -np ${NPROCS} -q -i data/training.liblinear -s model_05.txt -k 5
./linwrap --no-plot -np ${NPROCS} -q -i data/training.liblinear -s model_10.txt -k 10
./linwrap --no-plot -np ${NPROCS} -q -i data/training.liblinear -s model_20.txt -k 20
./linwrap --no-plot -np ${NPROCS} -q -i data/training.liblinear -s model_50.txt -k 50

./linwrap --no-plot -np ${NPROCS} -i data/training.liblinear -l model_01.txt \
          -o /tmp/test_01.txt 2>&1 | grep AUC
./linwrap --no-plot -np ${NPROCS} -i data/training.liblinear -l model_02.txt \
          -o /tmp/test_02.txt 2>&1 | grep AUC
./linwrap --no-plot -np ${NPROCS} -i data/training.liblinear -l model_05.txt \
          -o /tmp/test_05.txt 2>&1 | grep AUC
./linwrap --no-plot -np ${NPROCS} -i data/training.liblinear -l model_10.txt \
          -o /tmp/test_10.txt 2>&1 | grep AUC
./linwrap --no-plot -np ${NPROCS} -i data/training.liblinear -l model_20.txt \
          -o /tmp/test_20.txt 2>&1 | grep AUC
./linwrap --no-plot -np ${NPROCS} -i data/training.liblinear -l model_50.txt \
          -o /tmp/test_50.txt 2>&1 | grep AUC
