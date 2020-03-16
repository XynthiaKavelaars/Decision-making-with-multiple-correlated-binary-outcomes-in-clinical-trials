
#### 1. Uninformative prior - alpha = c(0.01,0.01,0.01,0.01) ####
#### 1.1 Correct specification ####
nMatFix1.1 <- array(NA, dim=c(nrow(Dists), length(Rules), nrow(pCutFix)))
nMatFix1.1[,,1] <- ceiling(nMat)

Fix1.1 <- vector("list", nrow(Dists))
for(i in 1:nrow(Dists)){
  for(sim in 1:nSim){
    Fix1.1[[i]][[sim]] <- MakeDecision(phiE=Dists[i,1:4], phiC=Dists[i,5:8],
                                       nInt=matrix(nMatFix1.1[i,,], nrow=length(Rules)), nDraw=nDraw, 
                                       alpha0E=alpha0E.1[i,], alpha0C=alpha0C.1[i,],
                                       type="FIX")
  }
}

Fix1.1[[7]][[2]][[1]]
AppendResults(Fix1.1, file="Workspaces/WorkspacesFix1.1.RData") 
AppendResults(nMatFix1.1, file="Workspaces/WorkspacesFix1.1.RData") 

#### 1.2 Underspecification ####
nMatFix1.2 <- array(NA, dim=c(nrow(Dists), length(Rules), nrow(pCutFix)))
nMatFix1.2[,,1] <- ceiling(nMat_under)

Fix1.2 <- vector("list", nrow(Dists))

for(i in 1:nrow(Dists)){
  for(sim in 1:nSim){
    Fix1.2[[i]][[sim]] <- MakeDecision(phiE=Dists[i,1:4], phiC=Dists[i,5:8],
                                       nInt=matrix(nMatFix1.2[i,,], nrow=length(Rules)), nDraw=nDraw, 
                                       alpha0E=alpha0E.1[i,], alpha0C=alpha0C.1[i,],
                                       type="FIX")
  }
}

AppendResults(Fix1.2, file="Workspaces/WorkspacesFix1.2.RData") 
AppendResults(nMatFix1.2, file="Workspaces/WorkspacesFix1.2.RData") 

#### 1.3 Overspecification ####
nMatFix1.3 <- array(NA, dim=c(nrow(Dists), length(Rules), nrow(pCutFix)))
nMatFix1.3[,,1] <- ceiling(nMat_over)

Fix1.3 <- vector("list", nrow(Dists))

for(i in 1:nrow(Dists)){
  for(sim in 1:nSim){
    Fix1.3[[i]][[sim]] <- MakeDecision(phiE=Dists[i,1:4], phiC=Dists[i,5:8],
                                       nInt=matrix(nMatFix1.3[i,,], nrow=length(Rules)), nDraw=nDraw, 
                                       alpha0E=alpha0E.1[i,], alpha0C=alpha0C.1[i,],
                                       type="FIX")
  }
}


AppendResults(Fix1.3, file="Workspaces/WorkspacesFix1.3.RData") 
AppendResults(nMatFix1.3, file="Workspaces/WorkspacesFix1.3.RData") 

#### 2. Jeffreys' prior ####
nMatFix2 <- array(NA, dim=c(nrow(Dists), length(Rules), nrow(pCutFix)))
nMatFix2[,,1] <- ceiling(nMat)

Fix2 <- vector("list", nrow(Dists))
for(i in 1:nrow(Dists)){
  for(sim in 1:nSim){
    Fix2[[i]][[sim]] <- MakeDecision(phiE=Dists[i,1:4], phiC=Dists[i,5:8],
                                       nInt=matrix(nMatFix2[i,,], nrow=length(Rules)), 
                                       nDraw=nDraw, 
                                       alpha0E=alpha0E.2[i,], alpha0C=alpha0C.2[i,],
                                       type="FIX")
  }
}

AppendResults(Fix2, file="Workspaces/WorkspacesFix2.RData") 
AppendResults(nMatFix2, file="Workspaces/WorkspacesFix2.RData") 

#### 3. Informative prior: delta0 = delta ####
nMatFix3 <- array(NA, dim=c(nrow(Dists), length(Rules), nrow(pCutFix)))
nMatFix3[,,1] <- ceiling(nMat)

Fix3 <- vector("list", nrow(Dists))
for(i in 1:nrow(Dists)){
  for(sim in 1:nSim){
    Fix3[[i]][[sim]] <- MakeDecision(phiE=Dists[i,1:4], phiC=Dists[i,5:8],
                                     nInt=matrix(nMatFix3[i,,], nrow=length(Rules)), 
                                     nDraw=nDraw, 
                                     alpha0E=alpha0E.3[i,], alpha0C=alpha0C.3[i,],
                                     type="FIX")
  }
}

AppendResults(Fix3, file="Workspaces/WorkspacesFix3.RData") 
AppendResults(nMatFix3, file="Workspaces/WorkspacesFix3.RData") 

#### 4. Informative prior: delta0 < delta ####
nMatFix4 <- array(NA, dim=c(nrow(Dists), length(Rules), nrow(pCutFix)))
nMatFix4[,,1] <- ceiling(nMat)

Fix4 <- vector("list", nrow(Dists))
for(i in 1:nrow(Dists)){
  for(sim in 1:nSim){
    Fix4[[i]][[sim]] <- MakeDecision(phiE=Dists[i,1:4], phiC=Dists[i,5:8],
                                     nInt=matrix(nMatFix4[i,,], length(Rules)), 
                                     nDraw=nDraw,
                                     alpha0E=alpha0E.4[i,], alpha0C=alpha0C.4[i,],
                                     type="FIX")
  }
}

AppendResults(Fix4, file="Workspaces/WorkspacesFix4.RData") 
AppendResults(nMatFix4, file="Workspaces/WorkspacesFix4.RData") 

#### 5. Informative prior: delta0 > delta ####
nMatFix5 <- array(NA, dim=c(nrow(Dists), length(Rules), nrow(pCutFix)))
nMatFix5[,,1] <- ceiling(nMat)

Fix5 <- vector("list", nrow(Dists))
for(i in 1:nrow(Dists)){
  for(sim in 1:nSim){
    Fix5[[i]][[sim]] <- MakeDecision(phiE=Dists[i,1:4], phiC=Dists[i,5:8],
                                     nInt=matrix(nMatFix5[i,,], nrow=length(Rules)), 
                                     nDraw=nDraw, 
                                     alpha0E=alpha0E.5[i,], alpha0C=alpha0C.5[i,],
                                     type="FIX")
  }
}

AppendResults(Fix5, file="Workspaces/WorkspacesFix5.RData") 
AppendResults(nMatFix5, file="Workspaces/WorkspacesFix5.RData") 

#### 6. Informative prior: delta0 = -delta ####
nMatFix6 <- array(NA, dim=c(nrow(Dists), length(Rules), nrow(pCutFix)))
nMatFix6[,,1] <- ceiling(nMat)

Fix6 <- vector("list", nrow(Dists))
for(i in 1:nrow(Dists)){
  for(sim in 1:nSim){
    Fix6[[i]][[sim]] <- MakeDecision(phiE=Dists[i,1:4], phiC=Dists[i,5:8],
                                     nInt=matrix(nMatFix6[i,,], nrow=length(Rules)), 
                                     nDraw=nDraw, 
                                     alpha0E=alpha0E.6[i,], alpha0C=alpha0C.6[i,],
                                     type="FIX")
  }
}

AppendResults(Fix6, file="Workspaces/WorkspacesFix6.RData") 
AppendResults(nMatFix6, file="Workspaces/WorkspacesFix6.RData") 
