function[probs] = getProbsLinearGP(meanfunc,covfunc,likfunc,x,y,test_x)
[n,m] = size(test_x);
hyp = [];
# Optimizamos los parametros de hyp
hyp2 = minimize(hyp,@gp,-40,@infVB,meanfunc,covfunc,likfunc,x,y)

# Obtenemos la prediccion
[a b c d lp] = gp(hyp,@infVB,meanfunc,covfunc,likfunc,x,y,test_x,ones(n,1));
probs = exp(lp);
endfunction