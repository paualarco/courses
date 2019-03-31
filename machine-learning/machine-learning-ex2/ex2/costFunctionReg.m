function [J, grad] = costFunctionReg(theta, X, y, lambda)
%COSTFUNCTIONREG Compute cost and gradient for logistic regression with regularization
%   J = COSTFUNCTIONREG(theta, X, y, lambda) computes the cost of using
%   theta as the parameter for regularized logistic regression and the
%   gradient of the cost w.r.t. to the parameters. 

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;

cost = costFunction(theta,X,y);

coste = size(cost);
sizeTheta = size(theta);
#t= size(sum(theta));
baseOnes = ones(size(theta));
baseOnes = theta(2:sizeTheta(1),:).^2;
R = ((lambda/(2*m)) * sum(baseOnes));
#size(cost)   Cost y R tienen que ser del mismo tamño
#size(R)
J = cost + R;


grad = zeros(size(theta));

x= sigmoid(X*theta);
grad1 = 1/m *(sum(X.*( x - y )));
#sizeTheta = size(theta)
#sizeSigmoid = size(transpose(grad1))  
#SizeTheta y sizeSigmoid tienen que ser del mismo tamaño

#Cramos una matriz de zeros de tamaño theta para poder hacer la suma entre grad1 y theta'
#Pero lo que queremos realmente es dejar el primer valor a 0, por lo tanto hacemos los
#calculos (lambda/m)*thetha para toda la matriz y luego le asignamos un zero a la 
#primera posición de la matriz
baseZerosGrad = zeros(sizeTheta);

baseZerosGrad = (lambda/m)*theta;
baseZerosGrad(1,1) = 0;


grad = transpose(grad1)+ baseZerosGrad;
# for (i = 2:iterations)
#    h = sigmoid(
#    grad(i,1) = cost + ((lambda/(m))*theta(i));
#  endfor


% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost of a particular choice of theta.
%               You should set J to the cost.
%               Compute the partial derivatives and set grad to the partial
%               derivatives of the cost w.r.t. each parameter in theta






% =============================================================

end
