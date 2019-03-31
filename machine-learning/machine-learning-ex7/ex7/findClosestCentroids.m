function idx = findClosestCentroids(X, centroids)
%FINDCLOSESTCENTROIDS computes the centroid memberships for every example
%   idx = FINDCLOSESTCENTROIDS (X, centroids) returns the closest centroids
%   in idx for a dataset X where each row is a single example. idx = m x 1 
%   vector of centroid assignments (i.e. each entry in range [1..K])
%

% Set K
K = size(centroids, 1);

% You need to return the following variables correctly.
idx = zeros(size(X,1), 1);

% ====================== YOUR CODE HERE ======================
% Instructions: Go over every example, find its closest centroid, and store
%               the index inside idx at the appropriate location.
%               Concretely, idx(i) should contain the index of the centroid
%               closest to example i. Hence, it should be a value in the 
%               range 1..K
%
% Note: You can use a for-loop over the examples to compute this.
%

sizeX = size(X);
sizeCentroids = size(centroids);
for example = 1:sizeX(1)
  c = -1;
  cAux = -1;
  for centroid = 1:sizeCentroids(1)
    if (centroid == 1) c = sum((X(example,:) - centroids(centroid,:)).^2); cIdx=centroid;
      else

      cAux = sum((X(example,:) - centroids(centroid,:)).^2);
    end
    if (cAux <= c && centroid != 1) 
          c = cAux;
          cIdx = centroid;
    end
    
  end
  idx(example) = cIdx;
end
      






% =============================================================

end

