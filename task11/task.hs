I.A. n=0
     length(replicate 0 x) =? 0

I.V. für n=k gilt length(replicate k x) = n

I.S. n=k+1
     length (replicate (k+1) x) =? (k+1)
     
     length(replicate (k+1) x) = length(x:replicate k x) -- replicate.2
                               = 1+length(replicate k x) -- length.2
                               = 1+k -- I.V.
                               = k+1
