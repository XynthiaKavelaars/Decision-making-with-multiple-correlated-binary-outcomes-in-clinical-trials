#### Input parameters simulation ####
nSim                                       <- 5e3 # Number of simulations
nDraw                                      <- 1e4 # Number of draws from posterior
nStep <-       c(seq(5,50,1), # Sample sizes at which evidence is evaluated if below criterion
                          seq(55,500,5))
 
#### Weights for Compensatory rule ####
w_uu <- round(OptimizeWeights(phiE=Dists[23,1:4], phiC=Dists[23,5:8], n=1e3),2)
w_uc <- round(OptimizeWeights(phiE=Dists[22,1:4], phiC=Dists[22,5:8], n=1e3),2)

#### Decision rules ####
Rules <- list(single=function(Delta){mean(rowSums(Delta[,c(1,2)])>0)},
              any = function(Delta){max(mean(rowSums(Delta[,c(1,2)])>0),
                                        mean(rowSums(Delta[,c(1,3)])>0))},
              all = function(Delta){min(mean(rowSums(Delta[,c(1,2)])>0),
                                        mean(rowSums(Delta[,c(1,3)])>0))},
              compE=function(Delta){mean(0.50*rowSums(Delta[,c(1,2)])+
                                           0.50*rowSums(Delta[,c(1,3)])>0)},
              compUU=function(Delta){mean(w_uu[1]*rowSums(Delta[,c(1,2)])+
                                            w_uu[2]*rowSums(Delta[,c(1,3)])>0)},
              compUC=function(Delta){mean(w_uc[1]*rowSums(Delta[,c(1,2)])+
                                            w_uc[2]*rowSums(Delta[,c(1,3)])>0)})

#### Prior hyperparameters alpha^0 ####
#### 1. Uninformative prior - alpha=c(0.01,0.01,0.01,0.01) ####
alpha0E.1 <- array(rep(0.01,4*nrow(Dists)), dim=c(nrow(Dists), 4))
alpha0C.1 <- array(rep(0.01,4*nrow(Dists)), dim=c(nrow(Dists), 4))

#### 2. Jeffreys' prior ####
alpha0E.2   <- array(rep(1/2,4*nrow(Dists)), dim=c(nrow(Dists), 4))
alpha0C.2   <- array(rep(1/2,4*nrow(Dists)), dim=c(nrow(Dists), 4))

#### 3. Informative prior - delta0 = delta ####
alpha0E.3 <- rep(20,4) * Dists[,1:4]
alpha0C.3 <- rep(20,4) * Dists[,5:8]
  
#### 4. Informative prior - delta0 < delta ####
alpha0E.4 <- rep(20,4) * Dists_under[,1:4]
alpha0C.4 <- rep(20,4) * Dists_under[,5:8]

#### 5. Informative prior - delta0 > delta ####
alpha0E.5 <- rep(20,4) * Dists_over[,1:4]
alpha0C.5 <- rep(20,4) * Dists_over[,5:8]

#### 6. Informative prior - delta0 = -delta ####
alpha0E.6 <- rep(20,4) * Dists[,5:8]
alpha0C.6 <- rep(20,4) * Dists[,1:4]


#### Decision criteria p_cut ####
pCutFix <- matrix(c(0.95,0.975,rep(0.95,4)), nrow=1)
pCutBAD1 <- matrix(c(0.99920,0.99980, 0.99830, 0.99960, 0.99870, 0.99910), nrow=1)
pCutBAD2 <- matrix(c(0.9963,0.9998, 0.9955, 0.9968, 0.9964, 0.9967), nrow=1)

GSD <- gsDesign(k=3, test.type=1, alpha=0.05, beta=0.20, timing=c(1/3,2/3,1))
nRatio <- GSD$n.I
Bounds <- gsBoundSummary(GSD)
pCutGSD <- matrix(rep(1 - Bounds[Bounds$Value == "p (1-sided)","Efficacy"], times=length(Rules)), nrow=length(nRatio), ncol=length(Rules))
pCutGSD[,2] <- pCutGSD[,2] + (1-pCutGSD[,2])/2