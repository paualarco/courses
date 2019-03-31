function p = predictOneVsAll(all_theta, X)
%PREDICT Predict the label for a trained one-vs-all classifier. The labels 
%are in the range 1..K, where K = size(all_theta, 1). 
%  p = PREDICTONEVSALL(all_theta, X) will return a vector of predictions
%  for each example in the matrix X. Note that X contains the examples in
%  rows. all_theta is a matrix where the i-th row is a trained logistic
%  regression theta vector for the i-th class. You should set p to a vector
%  of values from 1..K (e.g., p = [1; 3; 1; 2] predicts classes 1, 3, 1, 2
%  for 4 examples) 

m = size(X, 1)
sat = size(all_theta);
num_labels = size(all_theta, 1)

% You need to return the following variables correctly 
p = zeros(size(X, 1), 1);

% Add ones to the X data matrix
X = [ones(m, 1) X];

% ====================== YOUR CODE HERE ======================
% Instructions: Complete the following code to make predictions using
%               your learned logistic regression parameters (one-vs-all).
%               You should set p to a vector of predictions (from 1 to
%               num_labels).
%
% Hint: This code can be done all vectorized using the max function.
%       In particular, the max function can also return the index of the 
%       max element, for more information see 'help max'. If your examples 
%       are in rows, then, you can use max(A, [], 2) to obtain the max 
%       for each row.
%       

sx = size(X);
m
num_labels
P = zeros(m,num_labels);

for x = 1:m
  A = [];
  for i=1:num_labels
    #is= size(P(x,:))
   
    #Aqui estic multiplicant el vector de thetas de cada label, per cada training example
    #I els vaig afegint tots en un vector de tal manera que desrés pugui fer un max, i agafar el valor que més s'aproxima
    #Pero no entenc per que el valor més alt és el que més s'aproxima, ja que només estàs multiplicant la logical regression per el training example.
    #La pregunta és, per que com més alt el valor, significa que està més aprop del label/logistic regression??
    A = [A (X(x,:)*transpose(all_theta(i,:)))];
    #size(X(x,:)*transpose(all_theta(i,:)))
    
    
    
  endfor
   #A
   #maxA = max(A, [], 2)
   [value, position] = max (A);
   p(x,1)=position;
endfor






% =========================================================================


end
