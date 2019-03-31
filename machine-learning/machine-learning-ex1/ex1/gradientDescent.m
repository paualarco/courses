function [theta, J_history] = gradientDescent(X, y, theta, alpha, num_iters)
%GRADIENTDESCENT Performs gradient descent to learn theta
%   theta = GRADIENTDESCENT(X, y, theta, alpha, num_iters) updates theta by 
%   taking num_iters gradient steps with learning rate alpha

% Initialize some useful values
m = length(y); % number of training examples
J_history = zeros(num_iters, 1);


a= alpha;


for iter = 1:num_iters
  
  

  
  

    % ====================== YOUR CODE HERE ======================
    % Instructions: Perform a single gradient step on the parameter vector
    %               theta. 
    %
    % Hint: While debugging, it can be useful to print out the values
    %       of the cost function (computeCost) and gradient here.
    %


  x = X(:,1)*theta(1)+ X(:,2)*theta(2);
  i1 = (x - y);
  sum1 = sum(i1);
  temp1 = theta(1) - ((a/m)*sum1);
  
  i2 = i1 .* X(:,2);
  sum2 = sum(i2);
  temp2 =  theta(2) - ((a/m)*sum2);
  
  
   
  
  theta(1)=temp1;
  
  theta(2)=temp2;

  
  
    % ============================================================

    % Save the cost J in every iteration    
    J_history(iter) = computeCost(X, y, theta);

end

end
