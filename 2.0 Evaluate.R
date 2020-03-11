#### Extract results ####
#### 0. Initialization ####
# Scenario names 
scenario_names <- paste(rep(1:(nrow(Dists)/3), each=3), rep(c(".1", ".2", ".3"), times=nrow(Dists)/3), sep="")

Delta <- round(Dists[,1:4]-Dists[,5:8],2)

# Logical matrix with true scenarios
Truth <- data.frame(Single=c(1.00*rowSums(Delta[,c(1,2)])+0.00*rowSums(Delta[,c(1,3)])>0),
                    Any=c(1.00*rowSums(Delta[,c(1,2)])>0|1.00*rowSums(Delta[,c(1,3)])>0),
                    All=c(1.00*rowSums(Delta[,c(1,2)])>0&1.00*rowSums(Delta[,c(1,3)])>0),
                    CompE=c(0.50*rowSums(Delta[,c(1,2)])+0.50*rowSums(Delta[,c(1,3)])>0),
                    CompUU=c(w_uu[1]*rowSums(Delta[,c(1,2)])+w_uu[2]*rowSums(Delta[,c(1,3)])>0),
                    CompUC=c(w_uc[2]*rowSums(Delta[,c(1,2)])+w_uc[2]*rowSums(Delta[,c(1,3)])>0))

#### 1.0 Extract results BAD ####
WorkspacesBAD1 <- ObjectsBAD1 <-  rep(NA, nrow(Dists))
for(i in 1:nrow(Dists)){
  WorkspacesBAD1[i] <- paste0("Workspaces/WorkspacesBAD1.",i,".RData")
  ObjectsBAD1[i] <- paste0("BAD1.",i)
}

RejectBAD1 <- array(NA, dim=c(nrow(Dists), length(Rules), length(nStep), nSim))
nStopBAD1 <- array(NA, dim=c(nrow(Dists), length(Rules), nSim))
DeltaBAD1 <- array(NA, dim=c(nrow(Dists), nSim, length(Rules), 2))
for(i in 1:nrow(Dists)){
  load(WorkspacesBAD1[i])
  Res <- get(ObjectsBAD1[i])
  for(sim in 1:nSim){
    RejectBAD1[i,,,sim] <- apply(Res[[sim]][[1]], 2, function(x) x-pCutBAD1)>0
    nIndex <- apply(RejectBAD1[i,,,sim], 1, function(x) which(x==TRUE)[1])
    nStopBAD1[i,,sim] <- nStep[nIndex]
    for(rule in 1:length(Rules)){
      nIndex[rule] <- ifelse(is.na(nIndex[rule]), length(nStep), nIndex[rule])
      DeltaBAD1[i,sim,rule,] <-Res[[sim]][[2]][rule,nIndex[rule],]
    }
  }
  rm(Res)
}

AnyRejectBAD1 <- apply(RejectBAD1, c(1,2,4), function(x) any(x))
(pRejectBAD1 <- apply(AnyRejectBAD1, c(1,2), function(x) mean(x, na.rm=TRUE)))
nStopMeanBAD1 <- round(apply(nStopBAD1, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDBAD1 <- round(apply(nStopBAD1, c(1,2), sd)/sqrt(nSim), 2)
DeltaMeanBAD1 <- round(apply(DeltaBAD1, c(1,3,4), function(x) mean(x, na.rm=TRUE)), 2)

#### 2. Extract results FIX ####
#### 2.1.1 Uninformative prior - alpha = 0.01 - Correct specification ####
load("Workspaces/WorkspacesFIX1.1.RData")
dim(DeltaFIX1.1)
WhichRejectFIX1.1 <- nStopFIX_1.1 <- array(NA, dim=c(nrow(Dists), length(Rules), nSim))
DeltaFIX1.1 <- array(NA, dim=c(nrow(Dists), nSim, length(Rules), 2))
for(i in 1:nrow(Dists)){
  for(rule in 1:length(Rules)){
    for(sim in 1:nSim){
      Ind <- WhichRejectFIX1.1[i,rule,sim] <- which(Fix1.1[[i]][[sim]][[1]][rule,] - pCutFix[,rule] > 0)[1]
      Ind <- ifelse(is.na(Ind), nrow(pCutFix), Ind)
      DeltaFIX1.1[i,sim,rule,] <- Fix1.1[[i]][[sim]][[2]][rule,Ind,]
      nStopFIX_1.1[i,rule,sim] <- nMatFix1.1[i,rule,Ind]
    }
  }
}


pRejectFIX1.1 <- apply(!is.na(WhichRejectFIX1.1), c(1,2), function(x) mean(x, na.rm=TRUE))
nStopMeanFIX1.1 <- round(apply(nStopFIX_1.1, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDFIX1.1 <- round(apply(nStopFIX_1.1, c(1,2), sd)/sqrt(nSim), 2)
DeltaMeanFIX1.1 <- round(apply(DeltaFIX1.1, c(1,3,4), function(x) mean(x, na.rm=TRUE)), 2)

#### 2.1.2 Uninformative prior - alpha = 0.01 - Overspecification ####
load("Workspaces/WorkspacesFIX1.2.RData")
WhichRejectFIX1.2 <- nStopFIX_1.2 <- array(NA, dim=c(nrow(Dists), length(Rules), nSim))
DeltaFIX1.2 <- array(NA, dim=c(nrow(Dists), nSim, length(Rules), 2))
for(i in 1:nrow(Dists)){
  for(rule in 1:length(Rules)){
    for(sim in 1:nSim){
      Ind <- WhichRejectFIX1.2[i,rule,sim] <- which(Fix1.2[[i]][[sim]][[1]][rule,] - pCutFix[,rule] > 0)[1]
      Ind <- ifelse(is.na(Ind), nrow(pCutFix), Ind)
      DeltaFIX1.2[i,sim,rule,] <- Fix1.2[[i]][[sim]][[2]][rule,Ind,]
      nStopFIX_1.2[i,rule,sim] <- nMatFix1.2[i,rule,Ind]
    }
  }
}

pRejectFIX1.2 <- apply(!is.na(WhichRejectFIX1.2), c(1,2), function(x) mean(x, na.rm=TRUE))
nStopMeanFIX1.2 <- round(apply(nStopFIX_1.2, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDFIX1.2 <- round(apply(nStopFIX_1.2, c(1,2), sd)/sqrt(nSim), 2)
DeltaMeanFIX1.2 <- round(apply(DeltaFIX1.2, c(1,3,4), function(x) mean(x, na.rm=TRUE)), 2)
#rm(Fix1.2)

#### 2.1.3 Uninformative prior - alpha = 0.01 - Underspecification ####
load("Workspaces/WorkspacesFIX1.3.RData")
WhichRejectFIX1.3 <- nStopFIX_1.3 <- array(NA, dim=c(nrow(Dists), length(Rules), nSim))
DeltaFIX1.3 <- array(NA, dim=c(nrow(Dists), nSim, length(Rules), 2))
for(i in 1:nrow(Dists)){
  for(rule in 1:length(Rules)){
    for(sim in 1:nSim){
      Ind <- WhichRejectFIX1.3[i,rule,sim] <- which(Fix1.3[[i]][[sim]][[1]][rule,] - pCutFix[,rule] > 0)[1]
      Ind <- ifelse(is.na(Ind), nrow(pCutFix), Ind)
      DeltaFIX1.3[i,sim,rule,] <- Fix1.3[[i]][[sim]][[2]][rule,Ind,]
      nStopFIX_1.3[i,rule,sim] <- nMatFix1.3[i,rule,Ind]
    }
  }
}

pRejectFIX1.3 <- apply(!is.na(WhichRejectFIX1.3), c(1,2), function(x) mean(x, na.rm=TRUE))
nStopMeanFIX1.3 <- round(apply(nStopFIX_1.3, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDFIX1.3 <- round(apply(nStopFIX_1.3, c(1,2), sd)/sqrt(nSim), 2)
DeltaMeanFIX1.3 <- round(apply(DeltaFIX1.3, c(1,3,4), function(x) mean(x, na.rm=TRUE)), 6)
#rm(Fix1.3)

#### 2.2 Jeffreys' prior - Correct specification ####
load("Workspaces/WorkspacesFIX2.RData")
WhichRejectFIX2 <- nStopFIX_2 <- array(NA, dim=c(nrow(Dists), length(Rules), nSim))
DeltaFIX2 <- array(NA, dim=c(nrow(Dists), nSim, length(Rules), 2))
for(i in 1:nrow(Dists)){
  for(rule in 1:length(Rules)){
    for(sim in 1:nSim){
      Ind <- WhichRejectFIX2[i,rule,sim] <- which(Fix2[[i]][[sim]][[1]][rule,] - pCutFix[,rule] > 0)[1]
      Ind <- ifelse(is.na(Ind), nrow(pCutFix), Ind)
      DeltaFIX2[i,sim,rule,] <- Fix2[[i]][[sim]][[2]][rule,Ind,]
      nStopFIX_2[i,rule,sim] <- nMatFix2[i,rule,Ind]
    }
  }
}

pRejectFIX2 <- apply(!is.na(WhichRejectFIX2), c(1,2), function(x) mean(x, na.rm=TRUE))
nStopMeanFIX2 <- round(apply(nStopFIX_2, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDFIX2 <- round(apply(nStopFIX_2, c(1,2), sd)/sqrt(nSim), 2)
DeltaMeanFIX2 <- round(apply(DeltaFIX2, c(1,3,4), function(x) mean(x, na.rm=TRUE)), 2)

#### 2.3 Informative prior - delta0 = delta #### 


load("Workspaces/WorkspacesFIX3.RData")
WhichRejectFIX3 <- nStopFIX_3 <- array(NA, dim=c(nrow(Dists), length(Rules), nSim))
DeltaFIX3 <- array(NA, dim=c(nrow(Dists), nSim, length(Rules), 2))
for(i in 1:nrow(Dists)){
  for(rule in 1:length(Rules)){
    for(sim in 1:nSim){
      Ind <- WhichRejectFIX3[i,rule,sim] <- which(Fix3[[i]][[sim]][[1]][rule,] - pCutFix[,rule] > 0)[1]
      Ind <- ifelse(is.na(Ind), nrow(pCutFix), Ind)
      DeltaFIX3[i,sim,rule,] <- Fix3[[i]][[sim]][[2]][rule,Ind,]
      nStopFIX_3[i,rule,sim] <- nMatFix3[i,rule,Ind]
    }
  }
}

pRejectFIX3 <- apply(!is.na(WhichRejectFIX3), c(1,2), function(x) mean(x, na.rm=TRUE))
nStopMeanFIX3 <- round(apply(nStopFIX_3, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDFIX3 <- round(apply(nStopFIX_3, c(1,2), sd)/sqrt(nSim), 2)
DeltaMeanFIX3 <- round(apply(DeltaFIX3, c(1,3,4), function(x) mean(x, na.rm=TRUE)), 2)


#### 2.4 Informative prior - delta0 < delta ####

load("Workspaces/WorkspacesFIX4.RData")
WhichRejectFIX4 <- nStopFIX_4 <- array(NA, dim=c(nrow(Dists), length(Rules), nSim))
DeltaFIX4 <- array(NA, dim=c(nrow(Dists), nSim, length(Rules), 2))
for(i in 1:nrow(Dists)){
  for(rule in 1:length(Rules)){
    for(sim in 1:nSim){
      Ind <- WhichRejectFIX4[i,rule,sim] <- which(Fix4[[i]][[sim]][[1]][rule,] - pCutFix[,rule] > 0)[1]
      Ind <- ifelse(is.na(Ind), nrow(pCutFix), Ind)
      DeltaFIX4[i,sim,rule,] <- Fix4[[i]][[sim]][[2]][rule,Ind,]
      nStopFIX_4[i,rule,sim] <- nMatFix4[i,rule,Ind]
    }
  }
}

pRejectFIX4 <- apply(!is.na(WhichRejectFIX4), c(1,2), function(x) mean(x, na.rm=TRUE))
nStopMeanFIX4 <- round(apply(nStopFIX_4, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDFIX4 <- round(apply(nStopFIX_4, c(1,2), sd)/sqrt(nSim), 2)
DeltaMeanFIX4 <- round(apply(DeltaFIX4, c(1,3,4), function(x) mean(x, na.rm=TRUE)), 2)
rm(Fix4)

#### 2.5 Informative prior - delta0 > delta ####
load("Workspaces/WorkspacesFIX5.RData")
WhichRejectFIX5 <- nStopFIX_5 <- array(NA, dim=c(nrow(Dists), length(Rules), nSim))
DeltaFIX5 <- array(NA, dim=c(nrow(Dists), nSim, length(Rules), 2))
for(i in 1:nrow(Dists)){
  for(rule in 1:length(Rules)){
    for(sim in 1:nSim){
      Ind <- WhichRejectFIX5[i,rule,sim] <- which(Fix5[[i]][[sim]][[1]][rule,] - pCutFix[,rule] > 0)[1]
      Ind <- ifelse(is.na(Ind), nrow(pCutFix), Ind)
      DeltaFIX5[i,sim,rule,] <- Fix5[[i]][[sim]][[2]][rule,Ind,]
      nStopFIX_5[i,rule,sim] <- nMatFix5[i,rule,Ind]
    }
  }
}

pRejectFIX5 <- apply(!is.na(WhichRejectFIX5), c(1,2), function(x) mean(x, na.rm=TRUE))
nStopMeanFIX5 <- round(apply(nStopFIX_5, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDFIX5 <- round(apply(nStopFIX_5, c(1,2), sd)/sqrt(nSim), 2)
DeltaMeanFIX5 <- round(apply(DeltaFIX5, c(1,3,4), function(x) mean(x, na.rm=TRUE)), 2)

#### 2.6 Informative prior - delta0 = - delta ####
load("Workspaces/WorkspacesFIX6.RData")
WhichRejectFIX6 <- nStopFIX_6 <- array(NA, dim=c(nrow(Dists), length(Rules), nSim))
DeltaFIX6 <- array(NA, dim=c(nrow(Dists), nSim, length(Rules), 2))
for(i in 1:nrow(Dists)){
  for(rule in 1:length(Rules)){
    for(sim in 1:nSim){
      Ind <- WhichRejectFIX6[i,rule,sim] <- which(Fix6[[i]][[sim]][[1]][rule,] - pCutFix[,rule] > 0)[1]
      Ind <- ifelse(is.na(Ind), nrow(pCutFix), Ind)
     DeltaFIX6[i,sim,rule,] <- Fix6[[i]][[sim]][[2]][rule,Ind,]
      nStopFIX_6[i,rule,sim] <- nMatFix6[i,rule,Ind]
    }
  }
}

pRejectFIX6 <- apply(!is.na(WhichRejectFIX6), c(1,2), function(x) mean(x, na.rm=TRUE))
nStopMeanFIX6 <- round(apply(nStopFIX_6, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDFIX6 <- round(apply(nStopFIX_6, c(1,2), sd)/sqrt(nSim), 2)
DeltaMeanFIX6 <- round(apply(DeltaFIX6, c(1,3,4), function(x) mean(x, na.rm=TRUE)), 2)


#### 3. Extract results GSD ####
#### 3.1.1 Uninformative prior - alpha = 0.01 - Correct specification ####
load("Workspaces/WorkspacesGSD1.1.RData")
WhichRejectGSD1.1 <- nStopGSD1.1 <- nStopGSD1.1Reject <- array(NA, dim=c(nrow(Dists), length(Rules), nSim))
DeltaGSD1.1 <- array(NA, dim=c(nrow(Dists), nSim, length(Rules), 2))
for(i in 1:nrow(Dists)){
  for(rule in 1:length(Rules)){
    for(sim in 1:nSim){
      Ind <- WhichRejectGSD1.1[i,rule,sim] <- which(GSD1.1[[i]][[sim]][[1]][rule,] - pCutGSD[,rule] > 0)[1]
      Index <- ifelse(is.na(Ind), nrow(pCutGSD), Ind)
      DeltaGSD1.1[i,sim,rule,] <- GSD1.1[[i]][[sim]][[2]][rule,Index,]
      nStopGSD1.1[i,rule,sim] <- nMatGSD1.1[i,rule,Index]
      nStopGSD1.1Reject[i,rule,sim] <- nMatGSD1.1[i,rule,Ind]
    }
  }
}


pRejectGSD1.1 <- apply(!is.na(WhichRejectGSD1.1), c(1,2), function(x) mean(x, na.rm=TRUE))
nStopMeanGSD1.1 <- round(apply(nStopGSD1.1, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDGSD1.1 <- round(apply(nStopGSD1.1, c(1,2), sd)/sqrt(nSim), 2)
nStopMeanGSD1.1Reject <- round(apply(nStopGSD1.1Reject, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDGSD1.1Reject <- round(apply(nStopGSD1.1Reject, c(1,2), function(x) sd(x, na.rm=TRUE))/sqrt(nSim*pRejectGSD1.1), 2)
DeltaMeanGSD1.1 <- round(apply(DeltaGSD1.1, c(1,3,4), function(x) mean(x, na.rm=TRUE)), 2)
rm(GSD1.1)

#### 3.1.2 Uninformative prior - alpha = 0.01 - Overspecification ####
load("Workspaces/WorkspacesGSD1.2.RData")
WhichRejectGSD1.2 <- nStopGSD1.2 <- nStopGSD1.2Reject <- array(NA, dim=c(nrow(Dists), length(Rules), nSim))
DeltaGSD1.2 <- array(NA, dim=c(nrow(Dists), nSim, length(Rules), 2))
for(i in 1:nrow(Dists)){
  for(rule in 1:length(Rules)){
    for(sim in 1:nSim){
      Ind <- WhichRejectGSD1.2[i,rule,sim] <- which(GSD1.2[[i]][[sim]][[1]][rule,] - pCutGSD[,rule] > 0)[1]
      Index <- ifelse(is.na(Ind), nrow(pCutGSD), Ind)
      DeltaGSD1.2[i,sim,rule,] <- GSD1.2[[i]][[sim]][[2]][rule,Index,]
      nStopGSD1.2[i,rule,sim] <- nMatGSD1.2[i,rule,Index]
      nStopGSD1.2Reject[i,rule,sim] <- nMatGSD1.2[i,rule,Ind]
    }
  }
   }

pRejectGSD1.2 <- apply(!is.na(WhichRejectGSD1.2), c(1,2), function(x) mean(x, na.rm=TRUE))
nStopMeanGSD1.2 <- round(apply(nStopGSD1.2, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDGSD1.2 <- round(apply(nStopGSD1.2, c(1,2), sd)/sqrt(nSim), 2)
nStopMeanGSD1.2Reject <- round(apply(nStopGSD1.2Reject, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDGSD1.2Reject <- round(apply(nStopGSD1.2Reject, c(1,2), function(x) sd(x, na.rm=TRUE))/sqrt(nSim*pRejectGSD1.2), 2)
DeltaMeanGSD1.2 <- round(apply(DeltaGSD1.2, c(1,3,4), function(x) mean(x, na.rm=TRUE)), 2)
rm(GSD1.2)


#### 3.1.3 Uninformative prior - alpha = 0.01 - Underspecification ####
load("Workspaces/WorkspacesGSD1.3.RData")
WhichRejectGSD1.3 <- nStopGSD1.3 <- nStopGSD1.3Reject <- array(NA, dim=c(nrow(Dists), length(Rules), nSim))
DeltaGSD1.3 <- array(NA, dim=c(nrow(Dists), nSim, length(Rules), 2))
for(i in 1:nrow(Dists)){
  for(rule in 1:length(Rules)){
    for(sim in 1:nSim){
      Ind <- WhichRejectGSD1.3[i,rule,sim] <- which(GSD1.3[[i]][[sim]][[1]][rule,] - pCutGSD[,rule] > 0)[1]
      Index <- ifelse(is.na(Ind), nrow(pCutGSD), Ind)
      DeltaGSD1.3[i,sim,rule,] <- GSD1.3[[i]][[sim]][[2]][rule,Index,]
      nStopGSD1.3[i,rule,sim] <- nMatGSD1.3[i,rule,Index]
      nStopGSD1.3Reject[i,rule,sim] <- nMatGSD1.3[i,rule,Ind]
    }
  }
}

pRejectGSD1.3 <- apply(!is.na(WhichRejectGSD1.3), c(1,2), function(x) mean(x, na.rm=TRUE))
nStopMeanGSD1.3 <- round(apply(nStopGSD1.3, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDGSD1.3 <- round(apply(nStopGSD1.3, c(1,2), sd)/sqrt(nSim), 2)
nStopMeanGSD1.3Reject <- round(apply(nStopGSD1.3Reject, c(1,2), function(x) mean(x, na.rm=TRUE)), 0)
nStopSDGSD1.3Reject <- round(apply(nStopGSD1.3Reject, c(1,2), function(x) sd(x, na.rm=TRUE))/sqrt(nSim*pRejectGSD1.3), 2)
DeltaMeanGSD1.3 <- round(apply(DeltaGSD1.3, c(1,3,4), function(x) mean(x, na.rm=TRUE)), 2)
rm(GSD1.3)


