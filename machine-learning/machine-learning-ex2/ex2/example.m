function g = example()

  X = [1, 2, 3, 4, 5; 5, 4, 3, 2, 1;5, 6, 7, 8, 9; 1, 1, 1, 1, 1]
  g = zeros(size(X))
  if (X>=3) 
    g = 1
    endif
  #s = size(X)
 # for (i = 1:s(1))
  #  for (j = 1:s(2))
  #    if (X(i,j)>=3)
  #      g(i,j)=1;
        
 #     endif
#    endfor
#  endfor

end