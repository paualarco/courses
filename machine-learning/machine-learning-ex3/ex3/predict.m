function p = predict(Theta1, Theta2, X)
 
%PREDICT Predict the label of an input given a trained neural network
%   p = PREDICT(Theta1, Theta2, X) outputs the predicted label of X given the
%   trained weights of a neural network (Theta1, Theta2)

% Useful values
m = size(X, 1);

num_labels = size(Theta2, 1);
num_labels


% You need to return the following variables correctly 
p = zeros(size(X, 1), 1);
sizep = size(p)
X1 = [ones(size(X)(1),1),X;];
sizeX = size(X)
sizeT1 = size(Theta1)

z1 = X1 * transpose(Theta1);
a1= sigmoid(z1);
sizea1=size(a1)
a1 = [ones(size(a1)(1),1),a1];
sizeT2 = size(Theta2)
z2 = Theta2 * transpose(a1);
h = sigmoid(z2);

sizeh = size(h)
hTrans = transpose(h);


sizep1 = size(hTrans)(1);
for r = 1 : size(hTrans)(1)
  [maxValueofRow, index]  = max(hTrans(r,:));
  p(r) = index;
endfor
sizep = size(p)
%p = max(h, [], 2)
%sizep = size(p)

% ====================== YOUR CODE HERE ======================
% Instructions: Complete the following code to make predictions using
%               your learned neural network. You should set p to a 
%               vector containing labels between 1 to num_labels.
%
% Hint: The max function might come in useful. In particular, the max
%       function can also return the index of the max element, for more
%       information see 'help max'. If your examples are in rows, then, you
%       can use max(A, [], 2) to obtain the max for each row.
%









% =========================================================================


end
