#!/bin/bash

echo "Ejecutando prueba para HoeffdingTree"
java -cp moa.jar moa.DoTask \
"EvaluateModel -m 
(LearnModel -l trees.HoeffdingTree -s 
(generators.WaveformGenerator -i 2) -m 1000000) 
 -s (generators.WaveformGenerator -i 4)"

echo "Ejecutando prueba para HoeffdingAdaptiveTree"
java -cp moa.jar moa.DoTask \
"EvaluateModel -m 
(LearnModel -l trees.HoeffdingAdaptiveTree -s 
(generators.WaveformGenerator -i 2) -m 1000000)
 -s (generators.WaveformGenerator -i 4)"