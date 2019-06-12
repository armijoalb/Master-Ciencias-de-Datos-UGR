#!/bin/bash
#Local
#/opt/spark-2.2.0-bin-hadoop2.7/bin/spark-submit --master local[*] --class main.scala.runAlgorithms ./target/PracticaBigDataII-1.0-jar-with-dependencies.jar

#Cluster (.jar must be in cluster)
/opt/spark-2.2.0/bin/spark-submit --total-executor-cores 15 --executor-memory 7g --master spark://hadoop-master:7077 --class main.scala.runAlgorithms ./target/PracticaBigDataII-1.0-jar-with-dependencies.jar