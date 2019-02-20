function [U, S] = ArmijoRuizAlbertopca(X)
%APELLIDOSNOMBREPCA Run principal component analysis on the dataset X
%   [U, S, X] = ApellidosNombrepca(X) computes eigenvectors of the covariance matrix of X
%   Returns the eigenvectors U, the eigenvalues (on diagonal) in S
%

% Useful values
[m, n] = size(X);

% You need to return the following variables correctly.
U = zeros(n);
S = zeros(n);

% ====================== YOUR CODE HERE ======================
% Instructions: You should first compute the covariance matrix. Then, you
%               should use the "svd" function to compute the eigenvectors
%               and eigenvalues of the covariance matrix. 
%
% Note: When computing the covariance matrix, remember to divide by m (the
%       number of examples).
%

% INCLUYE TU CÓDIGO AQUÍ

% Calculamos la covarianza de los datos y lo almacenamos en sigma.
Sigma = cov(X);
% Calculamos la descomposición en valores singulares de dicha Sigma, con esto obtenemos
% los autovectores de X, que están almacenados en la matriz U. U tiene la misma dimesión que X,
% aunque luego solamente utilizamos sus K primeras columnas para obtener la proyección de los
% datos en K dimesiones en vez de las dimensiones que tiene X.
[U,S,V] = svd(Sigma);


% =========================================================================

end
