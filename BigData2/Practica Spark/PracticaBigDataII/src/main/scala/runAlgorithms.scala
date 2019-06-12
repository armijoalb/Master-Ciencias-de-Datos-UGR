package main.scala

import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.tree.{DecisionTree, RandomForest}
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.mllib.classification.kNN_IS.kNN_IS
import org.apache.spark.mllib.classification.{SVMModel, SVMWithSGD}
import org.apache.spark.mllib.evaluation.MulticlassMetrics
import org.apache.spark.mllib.evaluation.BinaryClassificationMetrics
import org.apache.spark.mllib.tree.PCARD
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.tree.GradientBoostedTrees
import org.apache.spark.mllib.tree.configuration.BoostingStrategy
import org.apache.spark.mllib.feature._
import org.apache.spark.mllib.linalg.{Vector, Vectors}
import java.io.PrintWriter


object runAlgorithms {

	def runROS(pathTrain:String, sc:SparkContext, majclass: String, minclass:String, overRate:Int = 1,
															numPartition:Int = 250):RDD[LabeledPoint] = {

  		val trainRaw = sc.textFile(pathTrain, numPartition).cache  
  
  		//println(trainRaw.first())
  		var oversample: RDD[String] = null
   		var fraction = 0.0 
    
	    val train_positive = trainRaw.filter(line => {

	  		val array = line.split(",")
	       	val label = array.last 
	      	label.compareToIgnoreCase(minclass) == 0
	      
	    })

    	val train_negative = trainRaw.filter(line => {

        	val array = line.split(",")
        	val label = array.last 
       		label.compareToIgnoreCase(majclass) == 0
       
    	})
    
	    val num_neg = train_negative.count()
	    val num_pos = train_positive.count()
  
	  	println("NUM_NEG" + num_neg)
	   	println("NUM_POS" + num_pos)
      
	    if (num_pos > num_neg){

	      	fraction = (num_pos*(overRate.toFloat/100)).toFloat/num_neg
	      	println("fraction:" + fraction)
	      	oversample = train_positive.union(train_negative.sample(true, fraction, 1234))
	      
	    }else{

	      	fraction = (num_neg*(overRate.toFloat/100)).toFloat/num_pos
	      	println("fraction:" + fraction)
	      	oversample = train_negative.union(train_positive.sample(true, fraction, 1234))
	    }
  
    	oversample.map{line =>
	      	val array = line.split(",")
	      	var arrayDouble = array.map(f => f.toDouble) 
	      	val featureVector = Vectors.dense(arrayDouble.init) 
	      	val label = arrayDouble.last 
	      	LabeledPoint(label, featureVector)
    	}
	}

	def runRUS(pathTrain:String, sc:SparkContext, majclass: String, minclass:String, 
															numPartition:Int = 250):RDD[LabeledPoint] = {					

  		val trainRaw = sc.textFile(pathTrain, numPartition).cache  
  
  		//println(trainRaw.first())
  		var undersample: RDD[String]= null
    	var fraction = 0.0 
    
	    val train_positive = trainRaw.filter(line => {
	        val array = line.split(",")
	        val label = array.last 
	      	label.compareToIgnoreCase(minclass) == 0
	    })

	    val train_negative = trainRaw.filter(line => {
	        val array = line.split(",")
	        val label = array.last 
	        label.compareToIgnoreCase(majclass) == 0
	    })
    
		val num_neg = train_negative.count()
		val num_pos = train_positive.count()

      
	    if (num_pos > num_neg){

	      fraction = num_neg.toFloat/num_pos
	      println("fraction:" + fraction)
	      undersample = train_negative.union(train_positive.sample(false, fraction, 1234))
	      
	    }else{

	      fraction = num_pos.toFloat/num_neg
	      println("fraction:" + fraction)
	      undersample = train_positive.union(train_negative.sample(false, fraction, 1234))
	    }
  
	    undersample.map{line =>
	      val array = line.split(",")
	      var arrayDouble = array.map(f => f.toDouble) 
	      val featureVector = Vectors.dense(arrayDouble.init) 
	      val label = arrayDouble.last 
	      LabeledPoint(label, featureVector)
	    }
	}

	def getMeasures(predictions: RDD[(Double, Double)], predName: String):Unit = {

		val metrics = new MulticlassMetrics(predictions)
		val precision = metrics.precision
		// val cm = metrics.confusionMatrix
		
		val binaryMetrics = new BinaryClassificationMetrics(predictions)
		val AUC = binaryMetrics.areaUnderROC
		
		println(s"AUC $predName = $AUC")
		println(s"Precision $predName = $precision")

	}

	def printMeasures(predictions: RDD[(Double, Double)], predName: String):Unit = {

		val metrics = new MulticlassMetrics(predictions)
		val precision = metrics.precision
		// val cm = metrics.confusionMatrix
		
		val binaryMetrics = new BinaryClassificationMetrics(predictions)
		val AUC = binaryMetrics.areaUnderROC
		
		println(s"AUC $predName = $AUC")
		println(s"Precision $predName = $precision")

		val folder_name = "/home/cd26256219/results"+predName+".txt"

		val writer = new PrintWriter(folder_name)
    	writer.write("Precision " + predName + ": " + precision + "\nAUC " + predName + ": "  + AUC + "\n")
    	writer.close()

	}

	def runDecisionTree(train: RDD[LabeledPoint], test:RDD[LabeledPoint]):Unit = {
		// Train a DecisionTree model.
		//	Empty categoricalFeaturesInfo indicates all features are continuous.
		var numClasses = 2
		var categoricalFeaturesInfo = Map[Int, Int]()
		var impurity = "gini"
		var maxDepth = 5
		var maxBins = 32

		val modelDT = DecisionTree.trainClassifier(train, numClasses, categoricalFeaturesInfo, impurity, maxDepth, maxBins)

		// Evaluate model on test instances and compute test error
		val labelAndPredsDT = test.map { point =>
			val prediction = modelDT.predict(point.features)
			(point.label, prediction)
		}
		
		//getMeasures(labelAndPredsDT,"DecisionTree")
		printMeasures(labelAndPredsDT, "ROS75_HMEBD_FCNN_DecisionTree")
	}

	def runRandomForest(train: RDD[LabeledPoint], test:RDD[LabeledPoint]):Unit = {
		// Train a RandomForest model.
		// Empty categoricalFeaturesInfo indicates all features are continuous.
		val numClasses = 2
		val categoricalFeaturesInfo = Map[Int, Int]()
		val numTrees = 300
		val featureSubsetStrategy = "auto" // Let the RDD[LabeledPoint]rithm choose.
		val impurity = "gini"
		val maxDepth = 10
		val maxBins = 32

		val modelRF = RandomForest.trainClassifier(train, numClasses, categoricalFeaturesInfo, numTrees, featureSubsetStrategy, impurity, maxDepth, maxBins)

		// Evaluate model on test instances and compute test error
		val labelAndPredsRF = test.map { point =>
			val prediction = modelRF.predict(point.features)
			(point.label, prediction)
		}

		printMeasures(labelAndPredsRF, "ROS75_HMEBD_FCNN_RandomForest")
	}

	def runPCARD(train: RDD[LabeledPoint], test:RDD[LabeledPoint], sc:SparkContext):Unit = {

		val cuts = 5
		val trees = 10
		val pcardTrain = PCARD.train(train, trees, cuts)
		val pcard = pcardTrain.predict(test)
		val labels = test.map(_.label).collect()
		val predsAndLabelsPCARD = sc.parallelize(pcard).zipWithIndex.map { case (v, k) => (k, v) }.join(test.zipWithIndex.map { case (v, k) => (k, v.label) }).map(_._2)
		
		printMeasures(predsAndLabelsPCARD, "ROS75_HMEBD_FCNN_PCARD")
	}

	def runLSVM(train: RDD[LabeledPoint], test:RDD[LabeledPoint]):Unit = {
		
		// Run training RDD[LabeledPoint]rithm to build the model
		// val numIterations = 100
		val modelLSVM = SVMWithSGD.train(train, 100)

		// Compute raw scores on the test set.
		val scoreAndLabelsLSVM = test.map { point =>
			val score = modelLSVM.predict(point.features)
			(score, point.label)
		}

		// Get evaluation metrics.
		printMeasures(scoreAndLabelsLSVM, "ROS75_HMEBD_FCNN_LSVM")
	}

	def runkNN(train: RDD[LabeledPoint], test:RDD[LabeledPoint], sc:SparkContext):Unit = {
		
		val K = 5
		val distanceType = 2
		val numClass = 2
		val numFeatures = 10
		val numPartitions = 10
		val numReduces = 10
		val numIterations = 2
		val maxWeight = 5
		val knn = kNN_IS.setup(train, test, K, distanceType, numClass, numFeatures, numPartitions, numReduces, numIterations, maxWeight)
		val labelAndPredsRFkNN = knn.predict(sc)
		
		printMeasures(labelAndPredsRFkNN, "kNN")
	}

	def runGBT(train: RDD[LabeledPoint], test:RDD[LabeledPoint]):Unit = {
		
		// Train a GradientBoostedTrees model.
		// The defaultParams for Classification use LogLoss by default.
		val boostingStrategy = BoostingStrategy.defaultParams("Classification")
		boostingStrategy.numIterations = 3 // Note: Use more iterations in practice.
		boostingStrategy.treeStrategy.numClasses = 2
		boostingStrategy.treeStrategy.maxDepth = 5

		val model = GradientBoostedTrees.train(train, boostingStrategy)

		// Evaluate model on test instances and compute test error
		val labelAndPredsGBT = test.map { point =>
		  val prediction = model.predict(point.features)
		  (point.label, prediction)
		}

		printMeasures(labelAndPredsGBT, "ROS75_HMEBD_FCNN_GBT")
	}

	//Filtro de Ruido HME-BD
	def runHMEBD(train: RDD[LabeledPoint]):RDD[LabeledPoint] = {

		val nTrees = 100
		val maxDepthRF = 10
		val partitions = 10

		val hme_bd_model = new HME_BD(train, nTrees, partitions, maxDepthRF, 48151623)

		hme_bd_model.runFilter()
	}

	//Filtro de Ruido NCNEdit
	def runNCNEdit(train: RDD[LabeledPoint]):RDD[LabeledPoint] = {

		val k = 3 //number of neighbors
		val ncnedit_bd_model = new NCNEdit_BD(train, k)

		ncnedit_bd_model.runFilter()
	}

	//Filtro de Ruido RNG
	def runRNG(train: RDD[LabeledPoint]):RDD[LabeledPoint] = {

		val order = true // Order of the graph (true = first, false = second)
		val selType = true // Selection type (true = edition, false = condensation)

		val rng_bd_model = new RNG_BD(train, order, selType)

		rng_bd_model.runFilter()
	}

	//Seleccion de instancias FCNN
	def runFCNN(train: RDD[LabeledPoint]):RDD[LabeledPoint] = {

		val k = 3 //number of neighbors

		val fcnn_mr_model = new FCNN_MR(train, k)

		fcnn_mr_model.runPR()
	}

	//Seleccion de instancias SSMASFLSDE
	def runSSMASFLSDE(train: RDD[LabeledPoint]):RDD[LabeledPoint] = {

		val ssmasflsde_mr_model = new SSMASFLSDE_MR(train) 

		ssmasflsde_mr_model.runPR()
	}

	//Seleccion de instancias RMHC
	def runRMHC(train: RDD[LabeledPoint]):RDD[LabeledPoint] = {

		val p = 0.1 // Percentage of instances (max 1.0)
		val it = 100 // Number of iterations
		val k = 3 // Number of neighbors

		val rmhc_mr_model = new RMHC_MR(train, p, it, k, 48151623)

		rmhc_mr_model.runPR()
	}

	def main(arg: Array[String]) {

		//Basic setup
		val jobName = "Run Algorithms AAR"

		//Spark Configuration
		val conf = new SparkConf().setAppName(jobName)
		val sc = new SparkContext(conf)

		//Load train and test
		//val pathTrain = "/home/alberto/spark/datasets/susy-10k-tra.data"
		val pathTrainCluster = "/user/datasets/master/susy/susyMaster-Train.data"
		//val rawDataTrain = sc.textFile(pathTrain)
		val rawDataTrain = sc.textFile(pathTrainCluster,30)

		//val pathTest = "/home/alberto/spark/datasets/susy-10k-tst.data"
		val pathTestCluster = "/user/datasets/master/susy/susyMaster-Test.data"
		//val rawDataTest = sc.textFile(pathTest)
		val rawDataTest = sc.textFile(pathTestCluster,30)


		val train = rawDataTrain.map { line =>
			val array = line.split(",")
			val arrayDouble = array.map(f => f.toDouble)
			val featureVector = Vectors.dense(arrayDouble.init)
			val label = arrayDouble.last
			LabeledPoint(label, featureVector)
		}.cache

		train.count
		train.first

		val test = rawDataTest.map { line =>
			val array = line.split(",")
			val arrayDouble = array.map(f => f.toDouble)
			val featureVector = Vectors.dense(arrayDouble.init)
			val label = arrayDouble.last
			LabeledPoint(label, featureVector)
		}.cache

		test.count
		test.first

		//Class balance

		val classInfo = train.map(lp => (lp.label, 1L)).reduceByKey(_ + _).collectAsMap()
		println("Class Balance")
		println(classInfo)

		//Decision tree

		//runDecisionTree(train, test)

		//Random Forest

		//runRandomForest(train, test)

		//PCARD

		//runPCARD(train, test, sc)

		//kNN
		
		//runkNN(train, test, sc)

		//LSVM

		//runLSVM(train, test)

		//GBT

		//runGBT(train, test)

		// runHMEBD(train)

		// runNCNEdit(train)

		//val newTrain = runRUS(pathTrainCluster, sc, "1.0", "0.0")
		val newTrain = runROS(pathTrainCluster, sc, "1.0", "0.0", 75)
		newTrain.persist

		val no_noise = runHMEBD(newTrain)
		no_noise.persist

		val instance_selection = runFCNN(no_noise)
		instance_selection.persist

		runPCARD(instance_selection,test,sc)
		runRandomForest(instance_selection,test)
		runDecisionTree(instance_selection,test)
		runLSVM(instance_selection,test)
		runGBT(instance_selection,test)


		//runRUS(pathTrain, sc, "1.0", "0.0")

	}
}