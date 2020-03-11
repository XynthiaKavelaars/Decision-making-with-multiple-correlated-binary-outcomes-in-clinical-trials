#### 1.3 Simulate adaptive design ####

nMatBAD     <- array(rep(nStep, each=nrow(Dists)*length(Rules)), dim=c(nrow(Dists), length(Rules), length(nStep)))
AppendResults(nMatBAD, file="Workspaces/WorkspacesnMatBAD.RData") 



#### 1. Uninformative prior (alpha = 0.01) ####

#### 1.1 Condition 1 ####
BAD1.1 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.1[[sim]] <- MakeDecision(phiE=Dists[1,1:4], phiC=Dists[1,5:8],
                                nInt=matrix(nMatBAD[1,,], nrow=length(Rules)), nDraw=nDraw, 
                                alpha0E=alpha0E.1[1,], alpha0C=alpha0C.1[1,],
                                type="BAD")
}

AppendResults(BAD1.1, file="Workspaces/WorkspacesBAD1.1.RData") 
#### 1.2 Condition 2 ####
BAD1.2 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.2[[sim]] <- MakeDecision(phiE=Dists[2,1:4], phiC=Dists[2,5:8],
                                nInt=matrix(nMatBAD[2,,], nrow=length(Rules)), nDraw=nDraw, 
                                alpha0E=alpha0E.1[2,], alpha0C=alpha0C.1[2,],
                                type="BAD")
}

AppendResults(BAD1.2, file="Workspaces/WorkspacesBAD1.2.RData") 

#### 1.3 Condition 3 ####
BAD1.3 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.3[[sim]] <- MakeDecision(phiE=Dists[3,1:4], phiC=Dists[3,5:8],
                                nInt=matrix(nMatBAD[3,,], nrow=length(Rules)), nDraw=nDraw, 
                                alpha0E=alpha0E.1[3,], alpha0C=alpha0C.1[3,],
                                type="BAD")
}

AppendResults(BAD1.3, file="Workspaces/WorkspacesBAD1.3.RData") 

#### 1.4 Condition 4 ####
BAD1.4 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.4[[sim]] <- MakeDecision(phiE=Dists[4,1:4], phiC=Dists[4,5:8],
                                nInt=matrix(nMatBAD[4,,], nrow=length(Rules)), nDraw=nDraw, 
                                alpha0E=alpha0E.1[4,], alpha0C=alpha0C.1[4,],
                                type="BAD")
}

AppendResults(BAD1.4, file="Workspaces/WorkspacesBAD1.4.RData") 


#### 1.5 Condition 5 ####
BAD1.5 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.5[[sim]] <- MakeDecision(phiE=Dists[5,1:4], phiC=Dists[5,5:8],
                                nInt=matrix(nMatBAD[5,,], nrow=length(Rules)), nDraw=nDraw, 
                                alpha0E=alpha0E.1[5,], alpha0C=alpha0C.1[5,],
                                type="BAD")
}

AppendResults(BAD1.5, file="Workspaces/WorkspacesBAD1.5.RData") 

#### 1.6 Condition 6 ####
BAD1.6 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.6[[sim]] <- MakeDecision(phiE=Dists[6,1:4], phiC=Dists[6,5:8],
                                nInt=matrix(nMatBAD[6,,], nrow=length(Rules)), nDraw=nDraw, 
                                alpha0E=alpha0E.1[6,], alpha0C=alpha0C.1[6,],
                                type="BAD")
}

AppendResults(BAD1.6, file="Workspaces/WorkspacesBAD1.6.RData") 

#### 1.7 Condition 7 ####
BAD1.7 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.7[[sim]] <- MakeDecision(phiE=Dists[7,1:4], phiC=Dists[7,5:8],
                                nInt=matrix(nMatBAD[7,,], nrow=length(Rules)), nDraw=nDraw, 
                                alpha0E=alpha0E.1[7,], alpha0C=alpha0C.1[7,],
                                type="BAD")
}

AppendResults(BAD1.7, file="Workspaces/WorkspacesBAD1.7.RData") 

#### 1.8 Condition 8 ####
BAD1.8 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.8[[sim]] <- MakeDecision(phiE=Dists[8,1:4], phiC=Dists[8,5:8],
                                nInt=matrix(nMatBAD[8,,], nrow=length(Rules)), nDraw=nDraw, 
                                alpha0E=alpha0E.1[8,], alpha0C=alpha0C.1[8,],
                                type="BAD")
}

AppendResults(BAD1.8, file="Workspaces/WorkspacesBAD1.8.RData") 

#### 1.9 Condition 9 ####
BAD1.9 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.9[[sim]] <- MakeDecision(phiE=Dists[9,1:4], phiC=Dists[9,5:8],
                                nInt=matrix(nMatBAD[9,,], nrow=length(Rules)), nDraw=nDraw, 
                                alpha0E=alpha0E.1[9,], alpha0C=alpha0C.1[9,],
                                type="BAD")
}

AppendResults(BAD1.9, file="Workspaces/WorkspacesBAD1.9.RData") 

#### 1.10 Condition 10 ####
BAD1.10 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.10[[sim]] <- MakeDecision(phiE=Dists[10,1:4], phiC=Dists[10,5:8],
                                 nInt=matrix(nMatBAD[10,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[10,], alpha0C=alpha0C.1[10,],
                                 type="BAD")
}

AppendResults(BAD1.10, file="Workspaces/WorkspacesBAD1.10.RData") 

#### 1.11 Condition 11 ####
BAD1.11 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.11[[sim]] <- MakeDecision(phiE=Dists[11,1:4], phiC=Dists[11,5:8],
                                 nInt=matrix(nMatBAD[11,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[11,], alpha0C=alpha0C.1[11,],
                                 type="BAD")
}

AppendResults(BAD1.11, file="Workspaces/WorkspacesBAD1.11.RData") 

#### 1.12 Condition 12 ####
BAD1.12 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.12[[sim]] <- MakeDecision(phiE=Dists[12,1:4], phiC=Dists[12,5:8],
                                 nInt=matrix(nMatBAD[12,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[12,], alpha0C=alpha0C.1[12,],
                                 type="BAD")
}

AppendResults(BAD1.12, file="Workspaces/WorkspacesBAD1.12.RData") 

#### 1.13 Condition 13 ####
BAD1.13 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.13[[sim]] <- MakeDecision(phiE=Dists[13,1:4], phiC=Dists[13,5:8],
                                 nInt=matrix(nMatBAD[13,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[13,], alpha0C=alpha0C.1[13,],
                                 type="BAD")
}

AppendResults(BAD1.13, file="Workspaces/WorkspacesBAD1.13.RData") 

#### 1.14 Condition 14 ####
BAD1.14 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.14[[sim]] <- MakeDecision(phiE=Dists[14,1:4], phiC=Dists[14,5:8],
                                 nInt=matrix(nMatBAD[14,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[14,], alpha0C=alpha0C.1[14,],
                                 type="BAD")
}

AppendResults(BAD1.14, file="Workspaces/WorkspacesBAD1.14.RData") 

#### 1.15 Condition 15 ####
BAD1.15 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.15[[sim]] <- MakeDecision(phiE=Dists[15,1:4], phiC=Dists[15,5:8],
                                 nInt=matrix(nMatBAD[15,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[15,], alpha0C=alpha0C.1[15,],
                                 type="BAD")
}

AppendResults(BAD1.15, file="Workspaces/WorkspacesBAD1.15.RData") 

#### 1.16 Condition 16 ####
BAD1.16 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.16[[sim]] <- MakeDecision(phiE=Dists[16,1:4], phiC=Dists[16,5:8],
                                 nInt=matrix(nMatBAD[16,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[16,], alpha0C=alpha0C.1[16,],
                                 type="BAD")
}

AppendResults(BAD1.16, file="Workspaces/WorkspacesBAD1.16.RData") 

#### 1.17 Condition 17 ####
BAD1.17 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.17[[sim]] <- MakeDecision(phiE=Dists[17,1:4], phiC=Dists[17,5:8],
                                 nInt=matrix(nMatBAD[17,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[17,], alpha0C=alpha0C.1[17,],
                                 type="BAD")
}

AppendResults(BAD1.17, file="Workspaces/WorkspacesBAD1.17.RData") 

#### 1.18 Condition 18 ####
BAD1.18 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.18[[sim]] <- MakeDecision(phiE=Dists[18,1:4], phiC=Dists[18,5:8],
                                 nInt=matrix(nMatBAD[18,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[18,], alpha0C=alpha0C.1[18,],
                                 type="BAD")
}

AppendResults(BAD1.18, file="Workspaces/WorkspacesBAD1.18.RData") 

#### 1.19 Condition 19 ####
BAD1.19 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.19[[sim]] <- MakeDecision(phiE=Dists[19,1:4], phiC=Dists[19,5:8],
                                 nInt=matrix(nMatBAD[19,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[19,], alpha0C=alpha0C.1[19,],
                                 type="BAD")
}

AppendResults(BAD1.19, file="Workspaces/WorkspacesBAD1.19.RData") 

#### 1.20 Condition 20 ####
BAD1.20 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.20[[sim]] <- MakeDecision(phiE=Dists[20,1:4], phiC=Dists[20,5:8],
                                 nInt=matrix(nMatBAD[20,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[20,], alpha0C=alpha0C.1[20,],
                                 type="BAD")
}

AppendResults(BAD1.20, file="Workspaces/WorkspacesBAD1.20.RData") 

#### 1.21 Condition 21 ####
BAD1.21 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.21[[sim]] <- MakeDecision(phiE=Dists[21,1:4], phiC=Dists[21,5:8],
                                 nInt=matrix(nMatBAD[21,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[21,], alpha0C=alpha0C.1[21,],
                                 type="BAD")
}

AppendResults(BAD1.21, file="Workspaces/WorkspacesBAD1.21.RData") 

#### 1.22 Condition 22 ####
BAD1.22 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.22[[sim]] <- MakeDecision(phiE=Dists[22,1:4], phiC=Dists[22,5:8],
                                 nInt=matrix(nMatBAD[22,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[22,], alpha0C=alpha0C.1[22,],
                                 type="BAD")
}

AppendResults(BAD1.22, file="Workspaces/WorkspacesBAD1.22.RData") 

#### 1.23 Condition 23 ####
BAD1.23 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.23[[sim]] <- MakeDecision(phiE=Dists[23,1:4], phiC=Dists[23,5:8],
                                 nInt=matrix(nMatBAD[23,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[23,], alpha0C=alpha0C.1[23,],
                                 type="BAD")
}

AppendResults(BAD1.23, file="Workspaces/WorkspacesBAD1.23.RData") 

#### 1.24 Condition 24 ####
BAD1.24 <- vector("list", nSim)
for(sim in 1:nSim){
  BAD1.24[[sim]] <- MakeDecision(phiE=Dists[24,1:4], phiC=Dists[24,5:8],
                                 nInt=matrix(nMatBAD[24,,], nrow=length(Rules)), nDraw=nDraw, 
                                 alpha0E=alpha0E.1[24,], alpha0C=alpha0C.1[24,],
                                 type="BAD")
}

AppendResults(BAD1.24, file="Workspaces/WorkspacesBAD1.24.RData") 





