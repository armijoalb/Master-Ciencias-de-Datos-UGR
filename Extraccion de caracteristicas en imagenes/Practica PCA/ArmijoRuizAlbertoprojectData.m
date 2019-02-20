function Z = ArmijoRuizAlbertoprojectData(X, U, K)
%ArmijoRuizAlbertoPROJECTDATA Computes the reduced data representation when projecting only 
%on to the top k eigenvectors
%   Z = ArmijoRuizAlbertoprojectData(X, U, K) computes the projection of 
%   the normalized inputs X into the reduced dimensional space spanned by
%   the first K columns of U. It returns the projected examples in Z.
%

% You need to return the following variables correctly.
Z = zeros(size(X, 1), K);

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the projection of the data using only the top K 
%               eigenvectors in U (first K columns). 
%               For the i-th example X(i,:), the projection on to the k-th 
%               eigenvector is given as follows:
%                    x = X(i, :)';
%                    projection_k = x' * U(:, k);
%

% INCLUYE TU CÓDIGO AQUÍ
% Al utilizar solamente los k primeros autovectores, la matriz que obtenemos Z tiene K dimesiones
% en vez de tener las que tenía X, para este ejemplo X[50x2] * U_reducida[2x1] = Z[50x1]

U_reduce = U(:,1:K);
Z = X*U_reduce;

% =============================================================

end
