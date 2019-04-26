#!/bin/bash

echo "PRUEBA CON HOEFFDING TREE ADAPTATIVO\n" > resultados_adapt.txt

for i in {1..10}; do
java -cp moa.jar moa.DoTask \
"EvaluateModel -m (LearnModel -l trees.HoeffdingAdaptiveTree -s 
(generators.WaveformGenerator -i $i) -m 1000000) -s 
(generators.WaveformGenerator -i 4)" >> resultados_adapt.txt
done