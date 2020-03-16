#### Sample size functions ####

#### 1. Weighted linear combination (Single, Comp-E, Comp-UU, Comp-UC) ####
nLinear <- function(phiE, phiC,
                    w1, w2, 
                    alpha=0.05, beta=0.20){
  
  theta1E <- sum(phiE[c(1,2)])
  theta2E <- sum(phiE[c(1,3)])
  theta1C <- sum(phiC[c(1,2)])
  theta2C <- sum(phiC[c(1,3)])
  
  sigma2 <- function(theta1, theta2, phi, w1, w2){
    w1^2 * theta1 * (1-theta1) + 
      w2^2 * theta2 * (1-theta2) +
      2 * w1 * w2 * (phi[1] - theta1 * theta2)
  }
  
  mu <- function(theta1, theta2, w1, w2){
    w1 * theta1 + w2 * theta2
  }
  
  muE <- mu(theta1E, theta2E, w1, w2)
  muC <- mu(theta1C, theta2C, w1, w2)
  
  varE <- sigma2(theta1E, theta2E, phiE, w1, w2)
  varC <- sigma2(theta1C, theta2C, phiC, w1, w2)
  
  if(muE>muC){
    n <- (varE+varC)*((qnorm(1-alpha)+qnorm(1-beta))/(muE-muC))^2}
  else{n <- NA}
  return(ceiling(n))
  }


#### 2. All rule ####
nAll <- function(phiE, phiC, nMax, alpha=0.05, beta=0.20){
  theta1E <- sum(phiE[c(1,2)])
  theta2E <- sum(phiE[c(1,3)])
  theta1C <- sum(phiC[c(1,2)])
  theta2C <- sum(phiC[c(1,3)])
  
  var1E <- theta1E * (1-theta1E)
  var1C <- theta1C * (1-theta1C)
  var2E <- theta2E * (1-theta2E)
  var2C <- theta2C * (1-theta2C)
  
  rho12E <- (phiE[1] - theta1E * theta2E)/
    sqrt(theta1E * (1-theta1E) * 
           theta2E * (1-theta2E))
  rho12C <- (phiC[1] - theta1C * theta2C)/
    sqrt(theta1C * (1-theta1C) * 
           theta2C * (1-theta2C))
  rho.nml <- (rho12E * sqrt(var1E * var2E) + 
                rho12C * sqrt(var1C * var2C)) / 
    sqrt((var1E + var1C) * (var2E + var2C))
    
  Sigma <- matrix(c(1,rho.nml,rho.nml,1), nrow=2, ncol=2, byrow=TRUE)
    
    pwr <- rep(NA, nMax)
    mu <- matrix(NA, nrow=nMax, ncol=2)
    
    for(n in 1:nMax){
      
      theta1 <- n * (theta1E + theta1C) / (2*n)
      theta2 <- n * (theta2E + theta2C) / (2*n)
      se10 <- sqrt((1/n + 1/n) * theta1 * (1-theta1))
      se20 <- sqrt((1/n + 1/n) * theta2 * (1-theta2))
      Z1 <- (theta1E - theta1C) / se10
      Z2 <- (theta2E - theta2C) / se20
      
      se1 <- sqrt(var1E/n + var1C/n)
      se2 <- sqrt(var2E/n + var2C/n)
      
      mu <- c((theta1E - theta1C- se10 * qnorm(1-alpha)) / se1,
              (theta2E - theta2C - se20 * qnorm(1-alpha)) / se2)
      pwr[n] <- pmvnorm(lower=-Inf, upper=mu, corr=Sigma)
      
    }
    
    n <- which(pwr>(1-beta))[1] 
    return(n)
}

#### 3. Any rule ####
nAny <- function(phiE, phiC, nMax, alpha=0.05, beta=0.20){
  theta1E <- sum(phiE[c(1,2)])
  theta2E <- sum(phiE[c(1,3)])
  theta1C <- sum(phiC[c(1,2)])
  theta2C <- sum(phiC[c(1,3)])
  
  var1E <- theta1E * (1-theta1E)
  var1C <- theta1C * (1-theta1C)
  var2E <- theta2E * (1-theta2E)
  var2C <- theta2C * (1-theta2C)
  
  rho12E <- (phiE[1] - theta1E * theta2E)/
    sqrt(theta1E * (1-theta1E) * 
           theta2E * (1-theta2E))
  rho12C <- (phiC[1] - theta1C * theta2C)/
    sqrt(theta1C * (1-theta1C) * 
           theta2C * (1-theta2C))
  rho.nml <- (rho12E * sqrt(var1E * var2E) + 
                rho12C * sqrt(var1C * var2C)) / 
    sqrt((var1E + var1C) * (var2E + var2C))
  
 Sigma <- matrix(c(1,rho.nml,rho.nml,1), nrow=2, ncol=2, byrow=TRUE)
  
  
  pwr <- rep(NA, nMax)
  for(n in 1:nMax){
  theta1 <- n * (theta1E + theta1C) / (2*n)
  theta2 <- n * (theta2E + theta2C) / (2*n)
  se10 <- sqrt((1/n + 1/n) * theta1 * (1-theta1))
  se20 <- sqrt((1/n + 1/n) * theta2 * (1-theta2))
  Z1 <- (theta1E - theta1C) / se10
  Z2 <- (theta2E - theta2C) / se20
  
  se1 <- sqrt(var1E/n + var1C/n)
  se2 <- sqrt(var2E/n + var2C/n)
   
  mu <- c(qnorm(1-alpha/2)  - (theta1E - theta1C) / se1,
          qnorm(1-alpha/2)  - (theta2E - theta2C) / se2)
  pwr[n] <- 1-pmvnorm(lower=-Inf, upper=mu, corr=Sigma)
  }
  
  n <- which(pwr>(1-beta))[1] 
  return(n)
}
