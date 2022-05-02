#Linear congruential method is used here to generate pseudo-random numbers, where
#n = sample size, a = multiplier, c = increment with default value 0, m = modulus, and x0 = seed value.

rng = function(n, a, c = 0, m, x0){
  x = rep(0,n)
  x[1] = x0 # initialize seed
  for (i in 1:n){
    x[i+1] = ((a*(x[i])) + c) %% m
  }
  values = x/m
  return(values)
  
}

#set example values to test function
rng(50,172,13,30307,17218)
hist(rng(50,172,13,30307,17218)) #assess if distribution is uniform to test level of randomness


##########################################


#The inverse method is used here for RNG using density function f(x) = exp(-|x|)/2, 
#with -inf < x < inf.

inverse = function(n){
  sample.number = runif(n) # get a uniform sample based on the number of values we want from our input
  f = -log(abs(1 - (2 * sample.number))) # this is the inverse CDF of the function f(x)
  hist(f, probability = TRUE, freq = FALSE, xlab = "value", ylim = c(0, 1))
  curve(dexp(x, rate = 1), from = 0, to = 10, add = TRUE, col = 2)
}


#The reject method uses two inputs for RNG: M = max value of f(x) and 
#n = sample size, while using same density function f(x) = exp(-|x|)/2, with 
#-inf < x < inf.

reject = function(n, M){
  
  x = double(n) # storage space for accepted values
  i = 0 # initiate number
  repeat{
    t = runif(n - i, -15, 15) # generates n vectors 
    u = runif(n - i)
    a = (M * u <= exp(-abs(t))/2) # only collects the accepted values, logical
    sum = sum(a) # count accepted values
    if (sum > 0) 
      x[(i + 1):(sum + i)] = t[a]
    if ((sum + i) == n) # this continues the function until we reach the value of n, and then the function will stop once we have no more values
      break
    i = i + sum
  }
  x
}

## To calculate max value of x:
f = function(x){
  func = exp(-abs(x))/2
  return(func)
}
M = max(f(seq(-15, 15, by=0.01)))
##

reject(1000,M) #test function using n=1000