function [accuracy,specifity,recall,precision,f_score] = getmetrics(confusion_matrix)
TN = confusion_matrix(1,1);
TP = confusion_matrix(2,2);
FP = confusion_matrix(1,2);
FN = confusion_matrix(2,1);
accuracy = (TN+TP)/(TN+TP+FN+FP);
specifity = TN/(TN+FP);
recall = TP/(TP+FN);
precision = TP/(TP+FP);
f_score = (2*precision*recall)/(precision+recall);
end