#### Compute parameters of data generating mechanisms 

#### 1. True delta ####
#### 1.1 Specify delta, theta, rho ####
# Matrix with bivariate treatment differences 
delta <- matrix(c(-0.20, -0.20,
                   0.00,  0.00,
                   0.10,  0.10,
                   0.20,  0.20,
                   0.40,  0.40,
                   0.40,  0.00,
                   0.20, -0.40,
                   0.24,  0.08), ncol=2, byrow=TRUE)
colnames(delta) <- c("delta1", "delta2")

# Success probabilities (symmetric around c(0.5,0.5))
theta1 <- matrix(c(0.5+1/2*delta[,1],0.5-1/2*delta[,1]), ncol=2) 
theta2 <- matrix(c(0.5+1/2*delta[,2],0.5-1/2*delta[,2]), ncol=2)
colnames(theta1) <- colnames(theta2) <- c("thetaE", "thetaC")

# Matrix of delta, theta, rho
pars <- cbind(cbind(delta, theta1, theta2)[rep(1:nrow(delta), each = 3), ],
              rep(c(-0.3,0,0.3),times=nrow(delta)))
colnames(pars) <- c("delta1", "delta2", "thetaE1", "thetaC1", "thetaE2", "thetaC2", "rho")

#### 1.2 Reparametrize to Dirichlet parameters ####
# Find phi_11
# Reparametrize Olkin & Trikalinos (2015), Equation 2.5)
phiE11 <- (pars[,"rho"]*sqrt(pars[,"thetaE1"]*(1-pars[,"thetaE1"])*pars[,"thetaE2"]*(1-pars[,"thetaE2"]))
             +pars[,"thetaE1"]*pars[,"thetaE2"])
phiC11 <- (pars[,"rho"]*sqrt(pars[,"thetaC1"]*(1-pars[,"thetaC1"])*pars[,"thetaC2"]*(1-pars[,"thetaC2"]))
           +pars[,"thetaC1"]*pars[,"thetaC2"])

# Parameters bivariate beta distribution (\vec{\theta}, \phi_{11})
Pars <- cbind(pars[,c("delta1", "delta2", "rho", "thetaE1", "thetaE2")], phiE11, 
              pars[,c("thetaC1", "thetaC2")], phiC11)
colnames(Pars) <- c("delta1", "delta2", "rho", "thetaE1", "thetaE2", "phiE11", "thetaC1", "thetaC2", "phiC11")

# Parameters Dirichlet distribution (\vec{\phi}_{j}) 
phiE <- cbind(phiE11,
              Pars[,"thetaE1"]-phiE11,
              Pars[,"thetaE2"]-phiE11,
              1-rowSums(Pars[,c("thetaE1","thetaE2")])+phiE11)

phiC <- cbind(phiC11,
              Pars[,"thetaC1"]-phiC11,
              Pars[,"thetaC2"]-phiC11,
              1-rowSums(Pars[,c("thetaC1","thetaC2")])+phiC11)

# Combine Dirichlet parameters of treatment $E$ and $C$ into one matrix. 
Dists <- cbind(phiE,phiC)
colnames(Dists) <- c("phiE11", "phiE10", "phiE01", "phiE00",
                     "phiC11", "phiC10", "phiC01", "phiC00")



#### 2. Overspecification ####
#### 2.1 Specify delta, theta ,rho ####
# Matrix with treatment differences \vec{\delta}=(delta_{1},delta_{2})
delta_over <- delta + 0.10

# Success probabilities (symmetric around c(0.5,0.5))
theta1_over <- matrix(c(0.5+1/2*delta_over[,1],0.5-1/2*delta_over[,1]), ncol=2) 
theta2_over <- matrix(c(0.5+1/2*delta_over[,2],0.5-1/2*delta_over[,2]), ncol=2)
colnames(theta1) <- colnames(theta2) <- c("thetaE", "thetaC")

# Matrix of delta, theta, rho
pars_over <- cbind(cbind(delta_over, theta1_over, theta2_over)[rep(1:nrow(delta_over), each = 3), ],
              rep(c(-0.30,0,0.30),times=nrow(delta_over)))
colnames(pars_over) <- c("delta_over1", "delta_over2", "thetaE1", "thetaC1", "thetaE2", "thetaC2", "rho")

#### 2.2 Reparametrize to Dirichlet parameters ####
# Find phi via reparametrization of Olkin & Trikalinos (2015), Equation 2.5)
phiE11_over <- (pars_over[,"rho"]*sqrt(pars_over[,"thetaE1"]*(1-pars_over[,"thetaE1"])*pars_over[,"thetaE2"]*(1-pars_over[,"thetaE2"]))
           +pars_over[,"thetaE1"]*pars_over[,"thetaE2"])
phiC11_over <- (pars_over[,"rho"]*sqrt(pars_over[,"thetaC1"]*(1-pars_over[,"thetaC1"])*pars_over[,"thetaC2"]*(1-pars_over[,"thetaC2"]))
           +pars_over[,"thetaC1"]*pars_over[,"thetaC2"])

# Parameters bivariate beta distribution (\vec{\theta}, \phi_{11})
Pars_over <- cbind(pars_over[,c("delta_over1", "delta_over2", "rho", "thetaE1", "thetaE2")], phiE11_over, 
              pars_over[,c("thetaC1", "thetaC2")], phiC11_over)
colnames(Pars_over) <- c("delta_over1", "delta_over2", "rho", "thetaE1", "thetaE2", "phiE11", "thetaC1", "thetaC2", "phiC11")

# Parameters Dirichlet distribution (\vec{\phi}_{j}) 
phiE_over <- cbind(phiE11_over,
              Pars_over[,"thetaE1"]-phiE11_over,
              Pars_over[,"thetaE2"]-phiE11_over,
              1-rowSums(Pars_over[,c("thetaE1","thetaE2")])+phiE11_over)

phiC_over <- cbind(phiC11_over,
              Pars_over[,"thetaC1"]-phiC11_over,
              Pars_over[,"thetaC2"]-phiC11_over,
              1-rowSums(Pars_over[,c("thetaC1","thetaC2")])+phiC11_over)

# Combine parameters of treatment $E$ and $C$ into single matrix. 
Dists_over <- cbind(phiE_over,phiC_over)
colnames(Dists_over) <- c("phiE11", "phiE10", "phiE01", "phiE00",
                          "phiC11", "phiC10", "phiC01", "phiC00")


#### 3. Underspecification ####
#### 3.1 Specify delta, theta, rho ####
# Matrix with bivariate treatment differences
delta_under <- delta - 0.10

# Success probabilities (symmetric around c(0.5,0.5))
theta1_under <- matrix(c(0.5+1/2*delta_under[,1],0.5-1/2*delta_under[,1]), ncol=2) 
theta2_under <- matrix(c(0.5+1/2*delta_under[,2],0.5-1/2*delta_under[,2]), ncol=2)
colnames(theta1_under) <- colnames(theta2_under) <- c("thetaE", "thetaC")

# Matrix of delta, theta, rho
pars_under <- cbind(cbind(delta_under, theta1_under, theta2_under)[rep(1:nrow(delta_under), each = 3), ],
                   rep(c(-0.30,0,0.30),times=nrow(delta_under)))
colnames(pars_under) <- c("delta_under1", "delta_under2", "thetaE1", "thetaC1", "thetaE2", "thetaC2", "rho")

#### 3.2 Reparametrize to Dirichlet parameters ####
# Find phi via reparametrization of Olkin & Trikalinos (2015), Equation 2.5)
phiE11_under <- (pars_under[,"rho"]*sqrt(pars_under[,"thetaE1"]*(1-pars_under[,"thetaE1"])*pars_under[,"thetaE2"]*(1-pars_under[,"thetaE2"]))
                +pars_under[,"thetaE1"]*pars_under[,"thetaE2"])
phiC11_under <- (pars_under[,"rho"]*sqrt(pars_under[,"thetaC1"]*(1-pars_under[,"thetaC1"])*pars_under[,"thetaC2"]*(1-pars_under[,"thetaC2"]))
                +pars_under[,"thetaC1"]*pars_under[,"thetaC2"])

# Parameters bivariate beta distribution (\vec{\theta}, \phi_{11})
Pars_under <- cbind(pars_under[,c("delta_under1", "delta_under2", "rho", "thetaE1", "thetaE2")], phiE11_under, 
                   pars_under[,c("thetaC1", "thetaC2")], phiC11_under)
colnames(Pars_under) <- c("delta_under1", "delta_under2", "rho", "thetaE1", "thetaE2", "phiE11", "thetaC1", "thetaC2", "phiC11")

# Parameters Dirichlet distribution (\vec{\phi}_{j}) 
phiE_under <- cbind(phiE11_under,
                   Pars_under[,"thetaE1"]-phiE11_under,
                   Pars_under[,"thetaE2"]-phiE11_under,
                   1-rowSums(Pars_under[,c("thetaE1","thetaE2")])+phiE11_under)

phiC_under <- cbind(phiC11_under,
                   Pars_under[,"thetaC1"]-phiC11_under,
                   Pars_under[,"thetaC2"]-phiC11_under,
                   1-rowSums(Pars_under[,c("thetaC1","thetaC2")])+phiC11_under)

# Combine parameters of treatment $E$ and $C$ into single matrix. 
Dists_under <- cbind(phiE_under,phiC_under)
colnames(Dists_under) <- c("phiE11", "phiE10", "phiE01", "phiE00",
                           "phiC11", "phiC10", "phiC01", "phiC00")

