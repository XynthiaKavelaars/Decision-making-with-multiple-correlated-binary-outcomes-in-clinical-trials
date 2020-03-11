# Sample size computations 

#### 0. Initialize ####
nMax <- 1e3                    # Maximum sample size for non-superiority data generating mechanisms

#### 1. Correct specification ####
nMat <- matrix(NA, nrow=nrow(Dists), ncol=length(Rules))

for(i in 1:nrow(Dists)){
  nMat[i,1] <- nLinear(phiE = Dists[i,1:4],
                       phiC = Dists[i,5:8],
                       w1 = 1.00, w2 = 0.00)
  nMat[i,2] <- nAny(phiE = Dists[i,1:4],
                    phiC = Dists[i,5:8],
                    nMax = nMax)
  nMat[i,3] <- nAll(phiE = Dists[i,1:4],
                    phiC = Dists[i,5:8],
                    nMax = nMax)
  nMat[i,4] <- nLinear(phiE = Dists[i,1:4],
                       phiC = Dists[i,5:8],
                       w1 = 0.50, w2 = 0.50)
  nMat[i,5] <- nLinear(phiE = Dists[i,1:4],
                       phiC = Dists[i,5:8],
                       w1 = 0.75, w2 = 0.25)
  nMat[i,6] <- nLinear(phiE = Dists[i,1:4],
                       phiC = Dists[i,5:8],
                       w1 = 0.62, w2 = 0.38)

}

nMat[is.na(nMat)] <- nMax
#### 2. Overspecification ####
nMat_over <- matrix(NA, nrow=nrow(Dists_over), ncol=length(Rules))

for(i in 1:nrow(Dists_over)){
  nMat_over[i,1] <- nLinear(phiE = Dists_over[i,1:4],
                       phiC = Dists_over[i,5:8],
                       w1 = 1.00, w2 = 0.00)
  nMat_over[i,2] <- nAny(phiE = Dists_over[i,1:4],
                    phiC = Dists_over[i,5:8],
                    nMax = nMax)
  nMat_over[i,3] <- nAll(phiE = Dists_over[i,1:4],
                    phiC = Dists_over[i,5:8],
                    nMax = nMax)
  nMat_over[i,4] <- nLinear(phiE = Dists_over[i,1:4],
                       phiC = Dists_over[i,5:8],
                       w1 = 0.50, w2 = 0.50)
  nMat_over[i,5] <- nLinear(phiE = Dists_over[i,1:4],
                       phiC = Dists_over[i,5:8],
                       w1 = 0.75, w2 = 0.25)
  nMat_over[i,6] <- nLinear(phiE = Dists_over[i,1:4],
                       phiC = Dists_over[i,5:8],
                       w1 = 0.62, w2 = 0.38)
  
  
}

nMat_over[is.na(nMat_over)] <- nMax

#### 3. Underspecification ####
nMat_under <- matrix(NA, nrow=nrow(Dists_under), ncol=length(Rules))

for(i in 1:nrow(Dists_under)){
  nMat_under[i,1] <- nLinear(phiE = Dists_under[i,1:4],
                            phiC = Dists_under[i,5:8],
                            w1 = 1.00, w2 = 0.00)
  nMat_under[i,2] <- nAny(phiE = Dists_under[i,1:4],
                          phiC = Dists_under[i,5:8],
                          nMax = nMax)
  nMat_under[i,3] <- nAll(phiE = Dists_under[i,1:4],
                          phiC = Dists_under[i,5:8],
                          nMax = nMax)
  nMat_under[i,4] <- nLinear(phiE = Dists_under[i,1:4],
                            phiC = Dists_under[i,5:8],
                            w1 = 0.50, w2 = 0.50)
  nMat_under[i,5] <- nLinear(phiE = Dists_under[i,1:4],
                            phiC = Dists_under[i,5:8],
                            w1 = 0.75, w2 = 0.25)
  nMat_under[i,6] <- nLinear(phiE = Dists_under[i,1:4],
                            phiC = Dists_under[i,5:8],
                            w1 = 0.62, w2 = 0.38)
  
  
}

nMat_under[is.na(nMat_under)] <- nMax

