% Práctica de Procesos Gaussianos en Matlab.
clear ; close all; clc
# Cargamos los datos.
load('Datos.mat');

# Creción de los folds
[prueba_test,labels_test] = createTestFold(Healthy_folds,Malign_folds,1);
[prueba_train,labels_train] = createTrainingFold(Healthy_folds,Malign_folds,1);
[prueba_test2,labels_test2] = createTestFold(Healthy_folds,Malign_folds,2);
[prueba_train2,labels_train2] = createTrainingFold(Healthy_folds,Malign_folds,2);
[prueba_test3,labels_test3] = createTestFold(Healthy_folds,Malign_folds,3);
[prueba_train3,labels_train3] = createTrainingFold(Healthy_folds,Malign_folds,3);
[prueba_test4,labels_test4] = createTestFold(Healthy_folds,Malign_folds,4);
[prueba_train4,labels_train4] = createTrainingFold(Healthy_folds,Malign_folds,4);
[prueba_test5,labels_test5] = createTestFold(Healthy_folds,Malign_folds,5);
[prueba_train5,labels_train5] = createTrainingFold(Healthy_folds,Malign_folds,5);

# Modelos obtenidos con Kernel Lineal.
covfunc = @covLIN;
meanfunc = @meanZero;
likfunc = @likLogistic;

# Fold 1
probs = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train,labels_train,prueba_test);
probs2 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train2,labels_train2,prueba_test);
probs3 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train3,labels_train3,prueba_test);
probs4 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train4,labels_train4,prueba_test);
probs5 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train5,labels_train5,prueba_test);

mean_probs = getMeanProbs(probs,probs2,probs3,probs4,probs5);
Confusion1 = confusionmat(labels_test,mean_probs);
[acc1,spec1,recall1,prec1,fscore1] = getmetrics(Confusion1);
printf("accuracy:%f \nspecifity:%f \nrecall:%f \nprecision:%f \nf-score:%f \n",
            acc1,spec1,recall1,prec1,fscore1);

probabilities = [];
probabilities = horzcat(probabilities,probs);
probabilities = horzcat(probabilities,probs2);
probabilities = horzcat(probabilities,probs3);
probabilities = horzcat(probabilities,probs4);
probabilities = horzcat(probabilities,probs5);
mean_probs = mean(probabilities,dim=2);
save "linear_probs1.mat" mean_probs;
save "linear_prec1.mat"  prec1;
save "linear_recall1.mat" recall1;

# Fold 2
probs = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train,labels_train,prueba_test2);
probs2 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train2,labels_train2,prueba_test2);
probs3 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train3,labels_train3,prueba_test2);
probs4 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train4,labels_train4,prueba_test2);
probs5 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train5,labels_train5,prueba_test2);

mean_probs = getMeanProbs(probs,probs2,probs3,probs4,probs5);
Confusion2 = confusionmat(labels_test2,mean_probs);
[acc2,spec2,recall2,prec2,fscore2] = getmetrics(Confusion2);
printf("accuracy:%f \nspecifity:%f \nrecall:%f \nprecision:%f \nf-score:%f \n",
            acc2,spec2,recall2,prec2,fscore2);

probabilities = [];
probabilities = horzcat(probabilities,probs);
probabilities = horzcat(probabilities,probs2);
probabilities = horzcat(probabilities,probs3);
probabilities = horzcat(probabilities,probs4);
probabilities = horzcat(probabilities,probs5);
mean_probs = mean(probabilities,dim=2);            
save "linear_probs2.mat" mean_probs;
save "linear_prec2.mat"  prec2;
save "linear_recall2.mat" recall2;

# Fold 3
probs = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train,labels_train,prueba_test3);
probs2 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train2,labels_train2,prueba_test3);
probs3 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train3,labels_train3,prueba_test3);
probs4 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train4,labels_train4,prueba_test3);
probs5 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train5,labels_train5,prueba_test3);

mean_probs = getMeanProbs(probs,probs2,probs3,probs4,probs5);
Confusion3 = confusionmat(labels_test3,mean_probs);
[acc3,spec3,recall3,prec3,fscore3] = getmetrics(Confusion3);
printf("accuracy:%f \nspecifity:%f \nrecall:%f \nprecision:%f \nf-score:%f \n",
            acc3,spec3,recall3,prec3,fscore3);

probabilities = [];
probabilities = horzcat(probabilities,probs);
probabilities = horzcat(probabilities,probs2);
probabilities = horzcat(probabilities,probs3);
probabilities = horzcat(probabilities,probs4);
probabilities = horzcat(probabilities,probs5);
mean_probs = mean(probabilities,dim=2);           
save "linear_probs3.mat" mean_probs;
save "linear_prec3.mat"  prec3;
save "linear_recall3.mat" recall3;


# Fold 4
probs = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train,labels_train,prueba_test4);
probs2 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train2,labels_train2,prueba_test4);
probs3 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train3,labels_train3,prueba_test4);
probs4 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train4,labels_train4,prueba_test4);
probs5 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train5,labels_train5,prueba_test4);

mean_probs = getMeanProbs(probs,probs2,probs3,probs4,probs5);
Confusion4 = confusionmat(labels_test4,mean_probs);
[acc4,spec4,recall4,prec4,fscore4] = getmetrics(Confusion4);

printf("accuracy:%f \nspecifity:%f \nrecall:%f \nprecision:%f \nf-score:%f \n",
            acc4,spec4,recall4,prec4,fscore4);

probabilities = [];
probabilities = horzcat(probabilities,probs);
probabilities = horzcat(probabilities,probs2);
probabilities = horzcat(probabilities,probs3);
probabilities = horzcat(probabilities,probs4);
probabilities = horzcat(probabilities,probs5);
mean_probs = mean(probabilities,dim=2);            
save "linear_probs4.mat" mean_probs;
save "linear_prec4.mat"  prec4;
save "linear_recall4.mat" recall4;

# Fold 5
probs = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train,labels_train,prueba_test5);
probs2 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train2,labels_train2,prueba_test5);
probs3 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train3,labels_train3,prueba_test5);
probs4 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train4,labels_train4,prueba_test5);
probs5 = getProbsLinearGP(meanfunc,covfunc,likfunc,prueba_train5,labels_train5,prueba_test5);

mean_probs = getMeanProbs(probs,probs2,probs3,probs4,probs5);
Confusion5 = confusionmat(labels_test5,mean_probs);
[acc5,spec5,recall5,prec5,fscore5] = getmetrics(Confusion5);

printf("accuracy:%f \nspecifity:%f \nrecall:%f \nprecision:%f \nf-score:%f \n",
            acc5,spec5,recall5,prec5,fscore5);

                     
probabilities = [];
probabilities = horzcat(probabilities,probs);
probabilities = horzcat(probabilities,probs2);
probabilities = horzcat(probabilities,probs3);
probabilities = horzcat(probabilities,probs4);
probabilities = horzcat(probabilities,probs5);
mean_probs = mean(probabilities,dim=2);            
save "linear_probs5.mat" mean_probs;
save "linear_prec5.mat"  prec5;
save "linear_recall5.mat" recall5;
