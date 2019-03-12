function[probs] = getProbsRadialGP(hyp,meanfunc,covfunc,likfunc,x,y,test_x)
[n,m] = size(test_x);
# Optimizamos los parametros de hyp
hyp2 = minimize(hyp,@gp,-40,@infVB,meanfunc,covfunc,likfunc,x,y)

# Obtenemos la prediccion
[a b c d lp] = gp(hyp2,@infVB,meanfunc,covfunc,likfunc,x,y,test_x,ones(n,1));
probs = exp(lp);
endfunction