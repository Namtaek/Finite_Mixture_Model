# HW1 - Kwon Namtaek


# Data generation
n = 500; p = 0.3; mu1 = -2; mu2 = 2
x1 = rnorm(n, mu1, 1)
x2 = rnorm(n, mu2, 1)
z = rbinom(n, 1, p)
x = z * x1 + (1 - z) * x2


Kwon9 = function(x, ini_vector) {
  
  loglik = vector()
  mu1 = ini_vector[1]
  mu2 = ini_vector[2]
  p = ini_vector[3]
  param_vec = ini_vector
  t = 1
  old_loglik = 0
  new_loglik = sum(log(p/sqrt(2*pi) * exp(-((x-mu1)^2)/2) + (1-p)/sqrt(2*pi) * exp(-((x-mu2)^2)/2)))
  
  while (abs(old_loglik - new_loglik) > 1e-10) {
    
    old_loglik = sum(log(p/sqrt(2*pi) * exp(-((x-mu1)^2)/2) + (1-p)/sqrt(2*pi) * exp(-((x-mu2)^2)/2)))
    # print(old_loglik)
    denom = p/sqrt(2*pi)*exp(-((x-mu1))^2/2) + (1-p)/sqrt(2*pi)*exp(-((x-mu2)^2)/2)
    num1 = (-(x - mu1) * p / sqrt(2*pi) * exp(-((x - mu1)^2)/2))
    num2 = (-(x - mu2) * (1-p) / sqrt(2*pi) * exp(-((x - mu2)^2)/2))
    num3 = (exp(-((x - mu1)^2)/2) - exp(-((x - mu2)^2)/2)) / sqrt(2*pi)
  
    l_prime1 = sum(num1 / denom)
    l_prime2 = sum(num2 / denom)
    l_prime3 = sum(num3 / denom)
  
    num11 = p / sqrt(2*pi) * exp(-((x-mu1)^2)/2) + (x-mu1)^2 * p / sqrt(2*pi) * exp(-((x-mu1)^2)/2)
    num22 = (1-p) / sqrt(2*pi) * exp(-((x-mu2)^2)/2) + (x-mu2)^2 * (1-p) / sqrt(2*pi) * exp(-((x-mu2)^2)/2)
    num13 = -(x-mu1) / sqrt(2*pi) * exp(-((x-mu1)^2)/2)
    num23 = (x-mu2) / sqrt(2*pi) * exp(-((x-mu2)^2)/2)
  
    l_prime11 = sum((num11*denom - num1^2) / denom^2)
    l_prime22 = sum((num22*denom - num2^2) / denom^2)
    l_prime33 = sum(-num3^2 / denom^2)
    l_prime12 = l_prime21 = sum(-num1*num2 / denom^2)
    l_prime13 = l_prime31 = sum((num13*denom - num3*num1) / denom^2)
    l_prime23 = l_prime32 = sum((num23*denom - num2*num3) / denom^2)
  
    l_prime = c(l_prime1, l_prime2, l_prime3)
    l_primeprime = matrix(c(l_prime11, l_prime12, l_prime13, 
                            l_prime21, l_prime22, l_prime23, 
                            l_prime31, l_prime32, l_prime33), nrow = 3) # symmetric
  
    param_vec = param_vec - l_prime %*% solve(l_primeprime)
    t = t+1
    mu1 = param_vec[1]
    mu2 = param_vec[2]
    p = param_vec[3]
    new_loglik = sum(log(p/sqrt(2*pi) * exp(-((x-mu1)^2)/2) + (1-p)/sqrt(2*pi) * exp(-((x-mu2)^2)/2)))
  }
  
  est_mu1 = param_vec[1]; est_mu2 = param_vec[2] ; est_p = param_vec[3]
  print(paste0("Newton-Raphson is converged in ", t-1, "steps."))
  return(c(est_mu1,est_mu2,est_p))
}

set.seed(42)

ini_vector1 = c(-1, 1, 0.5)
Kwon9(x = x, ini_vector = ini_vector1)

ini_vector2 = c(-1, -0.6, 0.8)
Kwon9(x = x, ini_vector = ini_vector2)

ini_vector3 = c(1, -1, 0.1)
Kwon9(x = x, ini_vector = ini_vector3) ## Identifiablity problem rises!
