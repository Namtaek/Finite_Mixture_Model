## HW2 - EM Algorithm for GMM

set.seed(42)

n = 500; p = 0.3; mu1 = -2; mu2 = 2; sigma1 = 3; sigma2 = 1
x1 = rnorm(n, mu1, sqrt(sigma1))
x2 = rnorm(n, mu2, sqrt(sigma2))
z = rbinom(n, 1, p)
x = z*x1 + (1 - z)*x2

Kwon9 = function(x, ini) { 
  
  t = 0
  old_loglik = 0
  mu1 = ini[1]; mu2 = ini[2]; sigma1 = ini[3]; sigma2 = ini[4]; p = ini[5];
  
  new_loglik = sum(z * log(p * dnorm(x, mu1, sqrt(sigma1))) + (1-z) * log((1-p) * dnorm(x, mu2, sqrt(sigma2))))
  
  
  while (abs(old_loglik - new_loglik) > 1e-10) {
    
    old_loglik = new_loglik
    # E-step
    z = p * dnorm(x, mu1, sqrt(sigma1)) / (p * dnorm(x, mu1, sqrt(sigma1)) + (1-p) * dnorm(x, mu2, sqrt(sigma2)))
    # M-step
    p = mean(z)
    mu1 = sum(z*x) / sum(z)
    mu2 = sum((1-z)*x) / sum(1-z)
    sigma1 = sum(z*(x-mu1)^2) / sum(z)
    sigma2 = sum((1-z)*(x-mu2)^2) / sum(1-z)
    new_loglik = sum(z * log(p * dnorm(x, mu1, sqrt(sigma1))) + (1-z) * log((1-p) * dnorm(x, mu2, sqrt(sigma2))))
    t = t + 1
    
  }
  cat(paste0("EM converges in ", t, " steps."))
  param = c(mu1, mu2, sigma1, sigma2, p)
  return(param)
  }

result1 = Kwon9(x, c(-0.5, 0.5, 1, 1, 0.1))
result1

