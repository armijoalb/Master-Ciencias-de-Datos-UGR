#!/bin/bash

echo "PRUEBAS CON HOEFFDING TREE\n" > resultados.txt

for i in {1..10}; do
java -cp moa.jar moa.DoTask \
"EvaluateModel -m (LearnModel -l trees.HoeffdingTree -s 
(generators.WaveformGenerator -i $i) -m 1000000) -s 
(generators.WaveformGenerator -i 4)" >> resultados.txt
echo "\n semilla $i \n" >> resultados.txt
done