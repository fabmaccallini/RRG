# SSVI

# Heston-like surface
phi.heston <- function (theta, lambda) {
  
  if (lambda < (1 + abs(rho)) / 4) stop ("The value of lambda selected does not guarantee a surface free of static arbitrage")
  
  phi <- 1 / (lambda * theta) * (1 - (1 - exp(- lambda * theta)) / (lambda * theta))
  
  return (phi)
}

phi.powerlaw <- function (theta, eta, gamma) {
  
  if (gamma <= 0 | gamma > 0.5) stop ("The value of gamma selected does not guarantee a surface free of static arbitrage")
  if (eta * (1 + abs(rho)) > 2) stop ("The value of eta selected does not guarantee a surface free of static arbitrage")
  
  phi <- eta / (theta ^ gamma * (1 + theta) ^ (1 - gamma))
  # phi <- eta * theta ^ -gamma
  
  return (phi)
}

phi.sabr <- function (theta, nu) {
  
  if (nu * sqrt(theta) * (1 + abs(rho)) <= 4 | nu ^ 2 * (1 + abs(rho)) <= 4) stop ("The value of nu selected does not guarantee a surface free of static arbitrage")
  
  phi <- nu / sqrt(theta)
  
  return (phi)
}

ssvi.tvar <- function (k, t, rho, theta, FUN = 'phi.powerlaw', FUN.params) {
  tvat <- do.call(FUN, FUN.params)
}