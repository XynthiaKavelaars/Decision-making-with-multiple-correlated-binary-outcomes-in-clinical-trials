#### Simulate group sequential design ####
#### 1.1 Correct specification ####
nMatGSD1.1 <- array(NA, dim=c(nrow(Dists), length(Rules), nrow(pCutGSD)))
nMatGSD1.1[,,1] <- ceiling(nMat * nRatio[1])
nMatGSD1.1[,,2] <- ceiling(nMat * nRatio[2])
nMatGSD1.1[,,3] <- ceiling(nMat * nRatio[3])

GSD1.1 <- vector("list", nrow(Dists))

for(i in 1:nrow(Dists)){
  for(sim in 1:nSim){
    GSD1.1[[i]][[sim]] <- MakeDecision(phiE=Dists[i,1:4], phiC=Dists[i,5:8],
                                       nInt=nMatGSD1.1[i,,], nDraw=nDraw, 
                                       alpha0E=alpha0E.1[i,], alpha0C=alpha0C.1[i,],
                                       type="GSD")
  }
}


AppendResults(GSD1.1, file="Workspaces/WorkspacesGSD1.1.RData") 
AppendResults(nMatGSD1.1, file="Workspaces/WorkspacesGSD1.1.RData") 

#### 1.2 Underspecification ####
nMatGSD1.2 <- array(NA, dim=c(nrow(Dists), length(Rules), nrow(pCutGSD)))
nMatGSD1.2[,,1] <- ceiling(nMat_under * nRatio[1])
nMatGSD1.2[,,2] <- ceiling(nMat_under * nRatio[2])
nMatGSD1.2[,,3] <- ceiling(nMat_under * nRatio[3])

GSD1.2 <- vector("list", nrow(Dists))

for(i in 1:nrow(Dists)){
  for(sim in 1:nSim){
    GSD1.2[[i]][[sim]] <- MakeDecision(phiE=Dists[i,1:4], phiC=Dists[i,5:8],
                                       nInt=nMatGSD1.2[i,,], nDraw=nDraw, 
                                       alpha0E=alpha0E.1[i,], alpha0C=alpha0C.1[i,],
                                       type="GSD")
  }
}

AppendResults(GSD1.2, file="Workspaces/WorkspacesGSD1.2.RData") 
AppendResults(nMatGSD1.2, file="Workspaces/WorkspacesGSD1.2.RData") 

#### 1.3 Overspecification ####
nMatGSD1.3 <- array(NA, dim=c(nrow(Dists), length(Rules), nrow(pCutGSD)))
nMatGSD1.3[,,1] <- ceiling(nMat_over * nRatio[1])
nMatGSD1.3[,,2] <- ceiling(nMat_over * nRatio[2])
nMatGSD1.3[,,3] <- ceiling(nMat_over * nRatio[3])

GSD1.3 <- vector("list", nrow(Dists))

for(i in 1:nrow(Dists)){
  for(sim in 1:nSim){
    GSD1.3[[i]][[sim]] <- MakeDecision(phiE=Dists[i,1:4], phiC=Dists[i,5:8],
                                       nInt=nMatGSD1.3[i,,], nDraw=nDraw, 
                                       alpha0E=alpha0E.1[i,], alpha0C=alpha0C.1[i,],
                                       type="GSD")
  }
}

AppendResults(GSD1.3, file="Workspaces/WorkspacesGSD1.3.RData") 
AppendResults(nMatGSD1.3, file="Workspaces/WorkspacesGSD1.3.RData") 










