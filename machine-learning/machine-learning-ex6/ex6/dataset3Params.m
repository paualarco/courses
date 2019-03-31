function [C, sigma] = dataset3Params(X, y, Xval, yval)
%DATASET3PARAMS returns your choice of C and sigma for Part 3 of the exercise
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = DATASET3PARAMS(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

% You need to return the following variables correctly.

CAux = [0.01, 0.03, 0.1, 0.3,1, 3, 10, 30]
sigmaAux = [0.01, 0.03,0.1, 0.3, 1, 3, 10,30]
%C = 1;
%sigma = 0.3;
%x1 = [1 2 1]; x2 = [0 4 -1];
error = 1
sigma = 0.1
C = 0.1
Xval
yval
X
y
for i = CAux
   for j = sigmaAux
    model= svmTrain(X, y, i, @(x1, x2) gaussianKernel(x1, x2, j)); 

    predict = svmPredict(model,Xval);
    errorAux=mean(double(predict~=yval))
    error
    if (errorAux<error)
      error=errorAux
      sigma=j
      C=i
    endif
  endfor
endfor

% ====================== YOUR CODE HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%







% =========================================================================

end
