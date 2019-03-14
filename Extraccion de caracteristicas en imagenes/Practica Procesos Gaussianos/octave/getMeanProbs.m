function[mean_probs] = getMeanProbs(p1,p2,p3,p4,p5)
  theta = 0.5;
  probabilities = [];
  probabilities = horzcat(probabilities,p1);
  probabilities = horzcat(probabilities,p2);
  probabilities = horzcat(probabilities,p3);
  probabilities = horzcat(probabilities,p4);
  probabilities = horzcat(probabilities,p5);

  mean_probs = mean(probabilities,dim=2);
  mean_probs(mean_probs < theta) = -1;
  mean_probs(mean_probs >= theta) = 1;
end