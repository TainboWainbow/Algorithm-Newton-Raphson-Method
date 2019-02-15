########################### Installing package ########################################

# This package will let us find derivatives of a given function
if (!"numDeriv" %in% installed.packages()){
  install.packages("numDeriv")
  }
# Loading the package
library("numDeriv")

######################### Run the 'newton' function first #############################

newton <- function(f, a, b, tol = 1e-12, n = 1000) {
  ## f = given function, a = initial approximation, b = upper bound
  x0 <- a
  # we need to store the result of each iteration
  k <- n
  
  ## checking if the function of the given initial approximation or the upper bound
  ## result in zero:
  fa <- f(a)
  if(fa == 0.0) {
    return(a)
  }
  
  fb <- f(b)
  if(fb == 0.0) {
    return(b)
  }
  
  ## if the given variables a,b are not roots, then proceed to finding the root
  for (i in 1:n) {
    # calculating derivative 
    dx <- genD(func = f, x = x0)$D[1]
    # calculating next approximation
    x1 <- x0 - (f(x0) / dx)
    k[i] <- x1
    
    # if the difference between the two successive approximations is significantly small
    # then proceed
    if (abs(x1 - x0) < tol) {
      root.approx <- tail(k, n = 1)
      return( list('root approximation' = root.approx, 'iterations' = k) )
    } 
    # if not, then proceed to next 
    x0 <- x1
  }
  print("Too many iterations in method")
}

################################### Example  ##########################################

# We want to find the root of the function 
funn <- function(x){x^3 - 2*x - 5}
# To determine the 'a' and 'b' values, let's examine its graph
curve(funn, lwd=2, xlim=c(0,5),ylim=c(-10,10))
abline(h=0, col="blue", lty=2)
abline(v=0, col="blue", lty=2)

# We can see the curve intersects the blue horizontal line at around x=2      
# Let a=2 and b=2:
newton(funn, a=2, b=3)
# The output shows us that it took 5 iterations to obtain the root value
