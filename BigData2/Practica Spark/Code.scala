// /opt/spark-2.2.0-bin-hadoop2.7/bin/spark-shell --packages djgg:PCARD:1.3

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.linalg.{Vector, Vectors}


//Load train and test

val pathTrain = "file:////home/alberto/spark/datasets/susy-10k-tra.data"
val rawDataTrain = sc.textFile(pathTrain)

val pathTest = "file:////home/alberto/spark/datasets/susy-10k-tst.data"
val rawDataTest = sc.textFile(pathTest)

val train = rawDataTrain.map{line =>
    val array = line.split(",")
    var arrayDouble = array.map(f => f.toDouble) 
    val featureVector = Vectors.dense(arrayDouble.init) 
    val label = arrayDouble.last 
    LabeledPoint(label, featureVector)
}.persist

train.count
train.first

val test = rawDataTest.map { line =>
    val array = line.split(",")
    var arrayDouble = array.map(f => f.toDouble) 
    val featureVector = Vectors.dense(arrayDouble.init) 
    val label = arrayDouble.last 
    LabeledPoint(label, featureVector)
}.persist

test.count
test.first


//Class balance

val classInfo = train.map(lp => (lp.label, 1L)).reduceByKey(_ + _).collectAsMap()


//Decision tree

import org.apache.spark.mllib.tree.DecisionTree
import org.apache.spark.mllib.tree.model.DecisionTreeModel

// Train a DecisionTree model.
//  Empty categoricalFeaturesInfo indicates all features are continuous.
val numClasses = 2
val categoricalFeaturesInfo = Map[Int, Int]()
val impurity = "gini"
val maxDepth = 5
val maxBins = 32

val model = DecisionTree.trainClassifier(train, numClasses, categoricalFeaturesInfo, impurity, maxDepth, maxBins)

// Evaluate model on test instances and compute test error
val labelAndPreds = test.map { point =>
  val prediction = model.predict(point.features)
  (point.label, prediction)
}
val testAcc = 1 - labelAndPreds.filter(r => r._1 != r._2).count().toDouble / test.count()
println(s"Test Accuracy = $testAcc")


//Random Forest

import org.apache.spark.mllib.tree.RandomForest
import org.apache.spark.mllib.tree.model.RandomForestModel

// Train a RandomForest model.
// Empty categoricalFeaturesInfo indicates all features are continuous.
val numClasses = 2
val categoricalFeaturesInfo = Map[Int, Int]()
val numTrees = 100
val featureSubsetStrategy = "auto" // Let the algorithm choose.
val impurity = "gini"
val maxDepth = 4
val maxBins = 32

val model = RandomForest.trainClassifier(train, numClasses, categoricalFeaturesInfo, numTrees, featureSubsetStrategy, impurity, maxDepth, maxBins)

// Evaluate model on test instances and compute test error
val labelAndPreds = test.map { point =>
  val prediction = model.predict(point.features)
  (point.label, prediction)
}
val testAcc = 1 - labelAndPreds.filter(r => r._1 != r._2).count.toDouble / test.count()
println(s"Test Accuracy = $testAcc")


//PCARD

import org.apache.spark.mllib.tree.PCARD

val cuts = 5
val trees = 10

val pcardTrain = PCARD.train(train, trees, cuts)

val pcard = pcardTrain.predict(test)

val labels = test.map(_.label).collect()

var cont = 0

for (i <- labels.indices) {
  if (labels(i) == pcard(i)) {
    cont += 1
  }
}

val testAcc = cont / labels.length.toFloat

println(s"Test Accuracy = $testAcc")


val predsAndLabels = sc.parallelize(pcard).zipWithIndex.map{case (v,k) => (k,v)}.join(test.zipWithIndex.map{case (v,k) => (k,v.label)}).map(_._2)

//Metrics

import org.apache.spark.mllib.evaluation.MulticlassMetrics

val metrics = new MulticlassMetrics(predsAndLabels)
val precision = metrics.precision
val cm = metrics.confusionMatrix
