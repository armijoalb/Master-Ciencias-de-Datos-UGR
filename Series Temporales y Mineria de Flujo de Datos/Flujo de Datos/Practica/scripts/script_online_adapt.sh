#!/bin/bash

for i in {1..10}; do
java -cp moa.jar moa.DoTask \
"EvaluateInterleavedTestThenTrain -l 
trees.HoeffdingAdaptiveTree -s 
(generators.WaveformGenerator -i $i)
-i 1000000 -f 10000" >> resultados_online_adative$i.csv
done