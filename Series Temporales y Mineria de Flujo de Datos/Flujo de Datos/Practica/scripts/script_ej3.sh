#!/bin/bash

echo "Ejecutando sentencia para HoeffdingTree"
java -cp moa.jar moa.DoTask \
"EvaluateInterleavedTestThenTrain -l trees.HoeffdingTree -s 
 (generators.RandomRBFGeneratorDrift -s 0.001 -k 3 -r 1 -i 1 -a 7 -n 3)
  -i 2000000 -f 100000"

echo "Ejecuitando sentencia para HoeffdingAdaptiveTree"
java -cp moa.jar moa.DoTask \
"EvaluateInterleavedTestThenTrain -l trees.HoeffdingAdaptiveTree -s 
 (generators.RandomRBFGeneratorDrift -s 0.001 -k 3 -r 1 -i 1 -a 7 -n 3)
  -i 2000000 -f 100000"