#!/bin /bash

echo "Ejecutando script para generar diferentes semillas"

for i in {1..30}; do
echo "Creando ht$i.csv"
java -cp moa.jar moa.DoTask "EvaluateInterleavedTestThenTrain -l trees.HoeffdingTree \
-s (generators.RandomTreeGenerator -i $i) -i 1000000 -f 10000" > ht$i.csv
done

for i in {1..30}; do
echo "Creando nb$i.csv"
java -cp moa.jar moa.DoTask "EvaluateInterleavedTestThenTrain -l bayes.NaiveBayes \
-s (generators.RandomTreeGenerator -i $i) -i 1000000 -f 10000" > nb$i.csv
done
