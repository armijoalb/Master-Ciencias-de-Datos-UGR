#!/bin/bash

echo "Ejecutando sentencia para HoeffdingTree"
java -cp moa.jar moa.DoTask \
"EvaluatePrequential -l trees.HoeffdingTree
-e (WindowClassificationPerformanceEvaluator -w 1000) -s
(generators.RandomRBFGeneratorDrift -s 0.001 -k 3 -r 1 -i 1 -a 7 -n 3)
 -i 2000000"

echo "Ejecuitando sentencia para HoeffdingAdaptiveTree"
java -cp moa.jar moa.DoTask \
"EvaluatePrequential -l trees.HoeffdingAdaptiveTree
-e (WindowClassificationPerformanceEvaluator -w 1000) -s
(generators.RandomRBFGeneratorDrift -s 0.001 -k 3 -r 1 -i 1 -a 7 -n 3)
 -i 2000000"