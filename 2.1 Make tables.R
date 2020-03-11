#### 0 Table 1. Scenarios of interest ####

# Find indices to select scenarios of interest out of all simulated scenarios
indices.rho <- matrix(NA, nrow=nrow(delta), ncol=3)
for(i in 1:nrow(delta)){
  indices.rho[i,] <- which(apply(t(apply(Pars[,1:2], 1, function(x) abs(x-delta[i,])<0.01)), 1, function(x) all(x=="TRUE")))}
indices <- as.vector(t(indices.rho))

Conditions <- delta[rep(1:nrow(delta), each=3),]
# Add correlations and exact parameters to each treatment difference
scenario_names <- paste(rep(1:nrow(delta), each=3), rep(c(".1", ".2", ".3"), times=nrow(delta)), sep="")
scenarios <- cbind.data.frame(scenario_names,
                        c(rbind(delta[,1], rep(NA, nrow(delta)), rep(NA, nrow(delta)))),
                        c(rbind(delta[,2], rep(NA, nrow(delta)), rep(NA, nrow(delta)))),
                        rep(c(-0.30,0.00,0.30), times=nrow(delta)),
                        Pars[,4:9])


# Format table 
IndicesScenarios <- t(apply(scenarios, 1, function(x) grepl("NA",x)))
scenarios[IndicesScenarios] <- NA

#### 0.1 Print settings ####
addtorow.conditions <- list()
addtorow.conditions$pos <- list(0,3,6,9,12,15,18,21)
addtorow.conditions$command <- c("Scenario & $\\delta_{1}$ & $\\delta_{2}$ & $\\rho_{\\theta_{j,1}, \\theta_{j,2}}$ & 
                           $\\theta_{E,1}$ & $\\theta_{E,2}$ & $\\phi_{E,11}$ &
                           $\\theta_{C,1}$ & $\\theta_{C,2}$ & $\\phi_{C,11}$ \\\\\n",
                                 rep("\\multicolumn{10}{c}{ }\\\\\n", 7))

#### 0.2 Print table ####
print(xtable(scenarios, caption="Simulation conditions",
             label="tab:conditions", align="llrrrrrrrrr",
             digits=c(1,1,rep(2,9))),
      add.to.row=addtorow.conditions, caption.placement="top", 
      include.colnames=FALSE, include.rownames=FALSE,
      NA.string = " ")

#### 1. Compare decision rules (Fixed design) ####
#### 1.1 Table P superiority ####
#### 1.1.1 Indices calibration scenarios p_{cut} for bold printing ####
# Find scenario closest to p_{superiority}=0.05
c.rowind <- apply(pRejectFIX1.1, 2, function(x) which(abs(x-0.05)==min(abs(x-0.05))))
c.colind <- vector("list", length(c.rowind))
for(i in 1:length(c.colind)){
  c.colind[[i]] <- rep(i, length(c.rowind[[i]]))
}

# Transform indices to logical matrix where index of interest is TRUE, otherwise FALSE
c.ind <- matrix(FALSE, nrow=nrow(pRejectFIX1.1), ncol=ncol(pRejectFIX1.1))
c.ind[cbind(unlist(c.rowind),unlist(c.colind))] <- TRUE
c.indices <- cbind(rep(FALSE, nrow(pRejectFIX1.1)),
                   c.ind)[indices,]

# Format table
pSupRules <- data.frame(scenario_names, pRejectFIX1.1,stringsAsFactors = FALSE)
pSupRules[,2:ncol(pSupRules)] <- format(round(pSupRules[,2:ncol(pSupRules)], 3), nsmall = 3)
pSupRules <- sapply(pSupRules, as.character)

#### 1.1.1 Print settings ####
addtorow.SupRules <- list()
addtorow.SupRules$pos <- list(0,3,6,9,12,15,18,21)
addtorow.SupRules$command <- c(" &
                          \\multicolumn{1}{l}{Single} & 
                          \\multicolumn{1}{l}{Any} &
                          \\multicolumn{1}{l}{All} & 
                          \\multicolumn{1}{l}{C-E} & 
                          \\multicolumn{1}{l}{C-UU}&
                          \\multicolumn{1}{l}{C-UC}\\\\\n",
                             rep("\\multicolumn{7}{c}{ }\\\\\n", 7))


#### 1.1.2 Print table ####

printbold(xtable(pSupRules, caption="P(Conclude superiority) for different treatment differences, correlation structures, and decision rules.
                 Bold-faced values indicate the conditions with the least favorable values.",
             label="tab:CompareRules_pSup", align="llrrrrrr"),
      which=c.indices,add.to.row=addtorow.SupRules, caption.placement="top", 
      include.colnames=FALSE, type="latex",
      include.rownames=FALSE,
      NA.string = " ", sanitize.text.function=I, table.placement="!ht")
 
#### 1.2 Table average nStop ####
# Format table
nStopRules <- data.frame(scenario_names, nStopMeanFIX1.1)

# Overlay nStop matrix with Truth matrix to remove values 
nStopRules[,-c(1)][Truth==FALSE] <- "-"

# Detect minimum values of average n_{j,stop} per condition for bold printing of the most favorable decision rule (per condition)
Mins <- Mins.ind <- vector("list", 24)
pb <- matrix(FALSE, nrow=nrow(nStopMeanFIX1.1), ncol=ncol(nStopMeanFIX1.1))

# Create logical matrix to indicate row-wise minimum values of average n_{j,stop} and their standard errors
for(i in 1:nrow(nStopRules)){
  Mins[[i]] <- min(nStopMeanFIX1.1[i,which(nStopMeanFIX1.1[i,]*Truth[i,]>0)], na.rm=TRUE)
  
  if(Mins[[i]]!=Inf){
    Mins.ind[[i]] <- which(nStopMeanFIX1.1[i,]==Mins[[i]], arr.ind=TRUE)
    pb[i,Mins.ind[[i]]] <- TRUE}
}

pb.indices <- cbind(rep(FALSE, nrow(nStopMeanFIX1.1)), pb)  


#### 1.2.1 Print settings ####
addtorow.nStopRules <- list()
addtorow.nStopRules$pos <- list(0,3,6,9,12,15,18,21)
addtorow.nStopRules$command <- c(" &  
                              \\multicolumn{1}{l}{Single} & 
                              \\multicolumn{1}{l}{Any} &
                              \\multicolumn{1}{l}{All} & 
                              \\multicolumn{1}{l}{C-E} & 
                              \\multicolumn{1}{l}{C-UU}&
                              \\multicolumn{1}{l}{C-UC}\\\\\n",
                            rep("\\multicolumn{7}{l}{ }\\\\\n", 7))



#### 1.2.2 Print table ####
printbold(xtable(nStopRules, caption="Average sample size for different treatment differences, correlation structures, and decision rules.
                 Bold-faced values indicate the lowest sample size per data generating mechanism.",
             label="tab:CompareRules_nStop", align="llrrrrrr", digits=c(1,1,rep(0,6))),
          which=pb.indices, type="latex",
          add.to.row=addtorow.nStopRules, caption.placement="top", include.colnames=FALSE, include.rownames=FALSE, sanitize.text.function=I, table.placement="!ht")



#### 1.3. Table bias ####

Bias.Rules <- array(NA, dim=c(nrow(Dists), length(Rules)))
for(i in 1:length(Rules)){
  Bias.Rules[,i] <- apply(cbind(DeltaMeanFIX1.1[,i,1], DeltaMeanFIX1.1[,i,2])-Conditions, 1, function(x) sprintf("(%s)", paste(sprintf("%03.2f", x), collapse=", ")))
}

# Format table
BiasRules <- data.frame(scenario_names, Bias.Rules)

#### 1.3.1 Print settings ####
addtorow.BiasRules <- list()
addtorow.BiasRules$pos <- list(0,3,6,9,12,15,18,21)
addtorow.BiasRules$command <- c(
  "& \\multicolumn{1}{l}{Single} & 
                              \\multicolumn{1}{l}{Any} &
                              \\multicolumn{1}{l}{All} & 
                              \\multicolumn{1}{l}{C-E} & 
                              \\multicolumn{1}{l}{C-UU}&
                              \\multicolumn{1}{l}{C-UC}\\\\\n",
  rep("\\multicolumn{7}{l}{ }\\\\\n", 7))

#### 1.3.2 Print table ####
print(xtable(BiasRules, caption="Average bias for different conditions (see Table \\ref{tab:conditions}) and decision rules.",
             label="tab:CompareRules_bias", align="llrrrrrr"),
      na.string=" ",
      add.to.row=addtorow.BiasRules, caption.placement="top", include.colnames=FALSE, include.rownames=FALSE, sanitize.text.function=I, table.placement="!ht")



#### 2.0 Compare designs ####
#### 2.1 P(Conclude superiority) ####
pSupDesigns <- cbind(pRejectBAD1[,4], 
                        rep(NA,nrow(Dists)),
                        pRejectFIX1.1[,4], pRejectFIX1.2[,4], pRejectFIX1.3[,4], 
                        rep(NA,nrow(Dists)),
                        pRejectGSD1.1[,4], pRejectGSD1.2[,4], pRejectGSD1.3[,4])



# Format table
pSup.Designs <- data.frame(scenario_names, pSupDesigns)
pSup.Designs[,2:ncol(pSup.Designs)] <- format(round(pSup.Designs[,2:ncol(pSup.Designs)], 3), nsmall = 3)
pSup.Designs <- sapply(pSup.Designs, as.character)
pSup.Designs[,-1][is.na(pSupDesigns)] <- " "

#### 2.1.1 Print settings ####
addtorow.SupDesigns <- list()
addtorow.SupDesigns$pos <- list(0,0,3,6,9,12,15,18,21)
addtorow.SupDesigns$command <- c(#"Scenario & \\multicolumn{9}{l}{P(Conclude superiority)}\\\\\n",
                               "& 
                               \\multicolumn{2}{l}{AD} &
                               \\multicolumn{4}{l}{FD} &
                               \\multicolumn{3}{l}{SD}\\\\\n",

                               "& & & 
                               \\multicolumn{1}{l}{$\\bm{\\delta}^{n} = \\bm{\\delta}^{true}$} & 
                               \\multicolumn{1}{l}{$\\bm{\\delta}^{n} < \\bm{\\delta}^{true}$} &
                               \\multicolumn{1}{l}{$\\bm{\\delta}^{n} > \\bm{\\delta}^{true}$} & & 
                               \\multicolumn{1}{l}{$\\bm{\\delta}^{n} = \\bm{\\delta}^{true}$} & 
                               \\multicolumn{1}{l}{$\\bm{\\delta}^{n} < \\bm{\\delta}^{true}$} &
                               \\multicolumn{1}{l}{$\\bm{\\delta}^{n} > \\bm{\\delta}^{true}$}\\\\\n",
                          #"& \\multicolumn{1}{l}{Fix} & 
                               #\\multicolumn{1}{l}{Gsd} &
                               #\\multicolumn{1}{l}{Bad} & 
                               #\\multicolumn{1}{l}{ } &
                               #\\multicolumn{1}{l}{Fix} &
                               #\\multicolumn{1}{l}{Gsd} &
                               #\\multicolumn{1}{l}{ } &
                               #\\multicolumn{1}{l}{Fix} &
                               #\\multicolumn{1}{l}{Gsd}\\\\\n",
                               rep("\\multicolumn{10}{c}{ }\\\\\n", 7))
    

#### 2.1.2 Print table ####
print(xtable(pSup.Designs, caption="P(Conclude superiority) for different designs and anticipated treatment differences $\\bm{\\delta}^{n}$ after applying the Compensatory decision rule with equal weights.",
             label="tab:CompareDesigns_pSup", align="llrp{0.01\\textwidth}rrrp{0.01\\textwidth}rrr"),
      add.to.row=addtorow.SupDesigns, caption.placement="top", 
      include.colnames=FALSE, include.rownames=FALSE,
      NA.string = " ", sanitize.text.function=I, table.placement="!ht")


#### 2.2 Average nStop ####
nStopCompare <- cbind(nStopMeanBAD1[,4], 
                      rep(NA,nrow(Dists)),
                      nStopMeanFIX1.1[,4], nStopMeanFIX1.2[,4], nStopMeanFIX1.3[,4], 
                      rep(NA,nrow(Dists)),
                      nStopMeanGSD1.1[,4], nStopMeanGSD1.2[,4], nStopMeanGSD1.3[,4])

nStop.Compare <- cbind(scenario_names, nStopCompare)
for(i in c(2,4:6,8:10)){
nStop.Compare[,i][Truth[,4]==FALSE] <- "-"}

#### 2.2.1 Print settings ####
addtorow.nStopDesigns <- list()
addtorow.nStopDesigns$pos <- list(0,0,3,6,9,12,15,18,21)
addtorow.nStopDesigns$command <- c(#"Scenario & \\multicolumn{9}{l}{Average $n_{stop}$}\\\\\n",
                             "& \\multicolumn{2}{l}{AD} &
                             \\multicolumn{4}{l}{FD} &
                             \\multicolumn{3}{l}{SD}\\\\\n",
                             
                             
                             
                             "& & & 
                               \\multicolumn{1}{l}{$\\bm{\\delta}^{n} = \\bm{\\delta}^{true}$} & 
                               \\multicolumn{1}{l}{$\\bm{\\delta}^{n} < \\bm{\\delta}^{true}$} &
                               \\multicolumn{1}{l}{$\\bm{\\delta}^{n} > \\bm{\\delta}^{true}$} & & 
                               \\multicolumn{1}{l}{$\\bm{\\delta}^{n} = \\bm{\\delta}^{true}$} & 
                               \\multicolumn{1}{l}{$\\bm{\\delta}^{n} < \\bm{\\delta}^{true}$} &
                               \\multicolumn{1}{l}{$\\bm{\\delta}^{n} > \\bm{\\delta}^{true}$}\\\\\n",
                          #"& \\multicolumn{1}{l}{Fix} & 
                          #\\multicolumn{1}{l}{Gsd} &
                          #\\multicolumn{1}{l}{Bad} & 
                          #\\multicolumn{1}{l}{ } &
                          #\\multicolumn{1}{l}{Fix} &
                          #\\multicolumn{1}{l}{Gsd} &
                          #\\multicolumn{1}{l}{ } &
                          #\\multicolumn{1}{l}{Fix} &
                          #\\multicolumn{1}{l}{Gsd}\\\\\n",
                             rep("\\multicolumn{10}{c}{ }\\\\\n", 7))

#### 2.2.2 Print table ####
print(xtable(nStop.Compare, caption="Average sample size for different designs and anticipated treatment differences $\\bm{\\delta}^{n}$ after applying the Compensatory decision rule with equal weights.",
             label="tab:CompareDesigns_nStop", align="llrp{0.01\\textwidth}rrrp{0.01\\textwidth}rrr"),
      add.to.row=addtorow.nStopDesigns, caption.placement="top", 
      include.colnames=FALSE, include.rownames=FALSE,
      NA.string = " ", sanitize.text.function=I, table.placement="!ht")


#### 2.3 Average bias ####
biasDesigns <- cbind(apply(cbind(DeltaMeanBAD1[,4,1], DeltaMeanBAD1[,4,2])-Conditions, 1, function(x) sprintf("(%s)", paste(sprintf("%3.2f",x), collapse=", "))), 
                      rep(NA,nrow(Dists)),
                      apply(cbind(DeltaMeanFIX1.1[,4,1], DeltaMeanFIX1.1[,4,2])-Conditions, 1, function(x) sprintf("(%s)", paste(sprintf("%3.2f",x), collapse=", "))),  
                      apply(cbind(DeltaMeanFIX1.2[,4,1], DeltaMeanFIX1.2[,4,2])-Conditions, 1, function(x) sprintf("(%s)", paste(sprintf("%3.2f",x), collapse=", "))),
                      apply(cbind(DeltaMeanFIX1.3[,4,1], DeltaMeanFIX1.3[,4,2])-Conditions, 1, function(x) sprintf("(%s)", paste(sprintf("%3.2f",x), collapse=", "))), 
                      rep(NA,nrow(Dists)),
                      apply(cbind(DeltaMeanGSD1.1[,4,1], DeltaMeanGSD1.1[,4,2])-Conditions, 1, function(x) sprintf("(%s)", paste(sprintf("%3.2f",x), collapse=", "))),
                      apply(cbind(DeltaMeanGSD1.2[,4,1], DeltaMeanGSD1.2[,4,2])-Conditions, 1, function(x) sprintf("(%s)", paste(sprintf("%3.2f",x), collapse=", "))), 
                      apply(cbind(DeltaMeanGSD1.3[,4,1], DeltaMeanGSD1.3[,4,2])-Conditions, 1, function(x) sprintf("(%s)", paste(sprintf("%3.2f",x), collapse=", "))))



# Format table
bias.Designs <- data.frame(scenario_names, biasDesigns)
#bias.Designs <- data.frame(scenario_names, Biass, rep(NA, nrow(Dists)), biasDesigns)
#bias.Designs[,-1] <- format(round(bias.Designs[,-1], 2), nsmall = 2)
IndicesDesigns <- t(apply(bias.Designs, 1, function(x) grepl("NA",x)))
bias.Designs[IndicesDesigns] <- NA

#### 2.3.1 Print settings ####
addtorow.BiasDesigns <- list()
addtorow.BiasDesigns$pos <- list(0,0,3,6,9,12,15,18,21)
addtorow.BiasDesigns$command <- c(#"Scenario & \\multicolumn{9}{l}{P(Conclude superiority)}\\\\\n",
  #"& \\multicolumn{2}{l}{TRUE} &
  "& \\multicolumn{2}{l}{AD} &
                               \\multicolumn{4}{l}{FD} &
                               \\multicolumn{3}{l}{SD}\\\\\n",
  
  "& & & 
                               \\multicolumn{1}{l}{$\\bm{\\bias}^{n} = \\bm{\\bias}^{true}$} & 
                               \\multicolumn{1}{l}{$\\bm{\\bias}^{n} < \\bm{\\bias}^{true}$} &
                               \\multicolumn{1}{l}{$\\bm{\\bias}^{n} > \\bm{\\bias}^{true}$} & & 
                               \\multicolumn{1}{l}{$\\bm{\\bias}^{n} = \\bm{\\bias}^{true}$} & 
                               \\multicolumn{1}{l}{$\\bm{\\bias}^{n} < \\bm{\\bias}^{true}$} &
                               \\multicolumn{1}{l}{$\\bm{\\bias}^{n} > \\bm{\\bias}^{true}$}\\\\\n",
  #"& \\multicolumn{1}{l}{Fix} & 
  #\\multicolumn{1}{l}{Gsd} &
  #\\multicolumn{1}{l}{Bad} & 
  #\\multicolumn{1}{l}{ } &
  #\\multicolumn{1}{l}{Fix} &
  #\\multicolumn{1}{l}{Gsd} &
  #\\multicolumn{1}{l}{ } &
  #\\multicolumn{1}{l}{Fix} &
  #\\multicolumn{1}{l}{Gsd}\\\\\n",
  rep("\\multicolumn{10}{c}{ }\\\\\n", 7))

#### 2.3.2 Print table ####
print(xtable(bias.Designs, caption="Average bias for different trial designs and anticipated treatment differences $\\bm{\\bias}^{n}$ after applying the Compensatory decision rule with equal weights.",
             label="tab:CompareDesigns_bias", align="llrp{0.01\\textwidth}rrrp{0.01\\textwidth}rrr"),
      na.string=" ",
      add.to.row=addtorow.BiasDesigns, caption.placement="top", include.colnames=FALSE, include.rownames=FALSE, sanitize.text.function=I, table.placement="!ht")




#### 3.0 Compare prior distributions ####
#### 3.1 Table P superiority ####
pSupPrior <- cbind(pRejectFIX1.1[,4], pRejectFIX2[,4],pRejectFIX3[,4],pRejectFIX4[,4],pRejectFIX5[,4],pRejectFIX6[,4])

# Format table
pSup_Prior <- data.frame(scenario_names, pSupPrior)
pSup_Prior[,2:ncol(pSup_Prior)] <- format(round(pSup_Prior[,2:ncol(pSup_Prior)], 3), nsmall = 3)
pSup_Prior <- sapply(pSup_Prior, as.character)

#### 3.1.1 Print settings ####
addtorow.SupPrior <- list()
addtorow.SupPrior$pos <- list(0,3,6,9,12,15,18,21)
addtorow.SupPrior$command <- c(#"Scenario & \\multicolumn{5}{l}{P(Conclude Superiority)}\\\\\n",
                             " &
                          \\multicolumn{1}{l}{1} &
                          \\multicolumn{1}{l}{2} & 
                          \\multicolumn{1}{l}{3} &
                          \\multicolumn{1}{l}{4} & 
                          \\multicolumn{1}{l}{5} & 
                          \\multicolumn{1}{l}{6}\\\\\n",
                             rep("\\multicolumn{7}{c}{ }\\\\\n", 7))

#### 3.1.2 Print table ####
print(xtable(pSup_Prior, caption="P(Conclude superiority) for different prior specifications (see Table \\ref{tab:priors}) after applying the Compensatory decision rule with equal weights.",
             label="tab:ComparePriors_pSup", align="llrrrrrr"),
      add.to.row=addtorow.SupPrior, caption.placement="top", 
      include.colnames=FALSE, include.rownames=FALSE,
      NA.string = " ", sanitize.text.function=I, table.placement="!ht")

#### 3.2 Table bias ####
BiasPrior <- cbind(apply(cbind(DeltaMeanFIX1.1[,4,1], DeltaMeanFIX1.1[,4,2])-Conditions, 1, function(x) sprintf("(%s)", paste(sprintf("%3.2f",x), collapse=", "))),
                    apply(cbind(DeltaMeanFIX2[,4,1], DeltaMeanFIX2[,4,2])-Conditions, 1, function(x) sprintf("(%s)", paste(sprintf("%3.2f",x), collapse=", "))),
                    apply(cbind(DeltaMeanFIX3[,4,1], DeltaMeanFIX3[,4,2])-Conditions, 1, function(x) sprintf("(%s)", paste(sprintf("%3.2f",x), collapse=", "))),
                    apply(cbind(DeltaMeanFIX4[,4,1], DeltaMeanFIX4[,4,2])-Conditions, 1, function(x) sprintf("(%s)", paste(sprintf("%3.2f",x), collapse=", "))),
                    apply(cbind(DeltaMeanFIX5[,4,1], DeltaMeanFIX5[,4,2])-Conditions, 1, function(x) sprintf("(%s)", paste(sprintf("%3.2f",x), collapse=", "))),
                    apply(cbind(DeltaMeanFIX6[,4,1], DeltaMeanFIX6[,4,2])-Conditions, 1, function(x) sprintf("(%s)", paste(sprintf("%3.2f",x), collapse=", "))))


# Format table
Bias_Prior <- data.frame(scenario_names, BiasPrior)
IndicesPrior <- t(apply(Bias_Prior, 1, function(x) grepl("NA",x)))
Bias_Prior[IndicesPrior] <- NA

#### 3.2.1 Print settings ####
addtorow.BiasPrior <- list()
addtorow.BiasPrior$pos <- list(0,3,6,9,12,15,18,21)
addtorow.BiasPrior$command <- c(#"Scenario & \\multicolumn{6}{l}{Average $\\bm{\\Bias}$}\\\\\n",
  #" & 
  #\\multicolumn{1}{l}{Pop} &
  #   "&\\multicolumn{1}{l}{$\\bm{\\alpha}^0 = \\bm{0.01}$} &
  #  \\multicolumn{1}{l}{Jeffreys\\textsc{\\char13}} & 
  # \\multicolumn{1}{l}{$\\bm{\\Bias}^{0} > \\bm{\\Bias}$} &
  #  \\multicolumn{1}{l}{$\\bm{\\Bias}^{0} = \\bm{\\Bias}$} & 
  #  \\multicolumn{1}{l}{$\\bm{\\Bias}^{0} < \\bm{\\Bias}$} & 
  #  \\multicolumn{1}{l}{$\\bm{\\Bias}^{0} = - \\bm{\\Bias}$}\\\\\n",
  #" &  
  #  \\multicolumn{1}{l}{$\\Bias_{1}$} & 
  #  \\multicolumn{1}{l}{$\\Bias_{2}$} &
  #  \\multicolumn{1}{l}{$\\Bias_{1}$} & 
  #  \\multicolumn{1}{l}{$\\Bias_{2}$} &
  #  \\multicolumn{1}{l}{$\\Bias_{1}$} & 
  #  \\multicolumn{1}{l}{$\\Bias_{2}$} &
  #  \\multicolumn{1}{l}{$\\Bias_{1}$} & 
  #  \\multicolumn{1}{l}{$\\Bias_{2}$} &
  #  \\multicolumn{1}{l}{$\\Bias_{1}$} & 
  #  \\multicolumn{1}{l}{$\\Bias_{2}$} &
  #  \\multicolumn{1}{l}{$\\Bias_{1}$} & 
  #  \\multicolumn{1}{l}{$\\Bias_{2}$} \\\\\n",
  " &
                          \\multicolumn{1}{l}{1} &
                          \\multicolumn{1}{l}{2} & 
                          \\multicolumn{1}{l}{3} &
                          \\multicolumn{1}{l}{4} & 
                          \\multicolumn{1}{l}{5} & 
                          \\multicolumn{1}{l}{6}\\\\\n",
  rep("\\multicolumn{7}{l}{ }\\\\\n", 7))

#### 3.2.2 Print table ####
print(xtable(Bias_Prior, caption="Average bias for different prior specifications (see Table \\ref{tab:priors}) after applying the Compensatory decision rule with equal weights.",
             label="tab:ComparePriors_Bias", align="llrrrrrr"),
      na.string=" ",
      add.to.row=addtorow.BiasPrior, caption.placement="top", include.colnames=FALSE, include.rownames=FALSE, sanitize.text.function=I, table.placement="!ht")



