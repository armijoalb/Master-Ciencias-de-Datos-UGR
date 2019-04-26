#!/bin/bash

echo "Ejecutando prueba para HoeffdingTree"
java -cp moa.jar moa.DoTask \
"EvaluateInterleavedTestThenTrain -l 
trees.HoeffdingTree -s 
(generators.WaveformGenerator -i 2)
-i 1000000 -f 10000" > resultados_online.txt

echo "Ejecutando prueba para HoeffdingAdaptiveTree"
java -cp moa.jar moa.DoTask \
"EvaluateInterleavedTestThenTrain -l 
trees.HoeffdingAdaptiveTree -s 
(generators.WaveformGenerator -i 2)
-i 1000000 -f 10000" > resultados_online_adative.txt