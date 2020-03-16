
#### 1. Sample posterior draws ####
# C++ function to speed up SampleDir()

cppFunction("NumericVector Callrgamma(int n, double shape, double scale) { 
            return(rgamma(n, shape, scale)); }")

# Sample from Dirichlet distribution
# Function to draw a sample from the posterior Dirichlet distribution of treatment $j$ (i.e. $\vec{\phi}_{j}$). 

# Input:  nDraw:      Number of draws 
#         alpha:      Parameters of the Dirichlet distribution 

# Output: z:          Matrix with four columns and nDraw rows, where each row reflects one draw
#                     Columns ordered as c("phi11","phi10","phi01","phi00")
#                     Rows sum to 1

SampleDir                            <- function (nDraw, alpha){
  k = length(alpha)
  z = array(0, dim = c(nDraw, k))
  for (i in 1:k) {z[, i] = Callrgamma(nDraw, as.double(alpha[i]),1/1.0)}
  s                                  <- rowSums(z)
  z                                  <- apply(z,2,"/",s)
  return(z)}

#### 2. Make decision ####
# Function to compute evidence in favor of the decision rules specified in the Numerical Evaluation section. 
# The posterior probability is expressed in Equation [FILL IN]

# Input: phiE:        Vector of length 4 with joint response probabilities of treatment E ($(phi_{E,11}, phi_{E,10},phi_{E,01}, phi_{E,00}$)
#        phiC:        Vector of length 4 with joint response probabilities of treatment C ($(phi_{C,11}, phi_{C,10},phi_{C,01}, phi_{C,00}$)
#        nInt:        Vector of length 6 with sample sizes to evaluate evidence in favor of treatment E for all decision rules.
#        nDraw:       Number of posterior draws
#        nSim:        Number of simulations per condition
#        alpha0E      Vector of length 4 with prior hyperparameters of treatment E
#        alpha0C      Vector of length 4 with prior hyperparameters of treatment C

# Output: list of 7 elements:
#        PoP:        A no. of Rules x nSim matrix with posterior probabilities.
#                    Order of rows: c("Single", "Any", "All", "C-E", "C-UU", "C-UC")
#        MeanDelta:  A no. of Rules x 2 matrix with average posterior treatment differences for each rule.
#        MeanPhiE:   A no. of Rules x 4 matrix with average posterior joint response probabilities of treatment E for each rule.
#        MeanPhiC:   A no. of Rules x 4 matrix with average posterior joint response probabilities of treatment C for each rule.
#        SdDelta:    A no. of Rules x 2 matrix with standard devations of posterior treatment differences for each rule.
#        SdPhiE:     A no. of Rules x 4 matrix with standard devations of posterior joint response probabilities of treatment E for each rule.
#        SdPhiC:     A no. of Rules x 4 matrix with standard devations of posterior joint response probabilities of treatment E for each rule.


MakeDecision <- function(phiE, phiC, nInt, nDraw, alpha0E, alpha0C, type){
  PoP <- array(NA, dim=c(length(Rules), ncol(nInt)))
  MeanDelta <- array(NA, dim=c(length(Rules), ncol(nInt), 2))
  
    # Sample data
    datE <- rmultinom(max(nInt),1,phiE)
    datC <- rmultinom(max(nInt),1,phiC)
    
    # Compute joint response frequencies 
    sE <- apply(datE, 1, cumsum)
    sC <- apply(datC, 1, cumsum)
    
    for(int in 1:ncol(nInt)){
      
      if(type %in% c("GSD", "FIX")) {
        for(rule in 1:length(Rules)){
        # Sample size
        n <- nInt[rule,int]
        
      # Compute posterior alpha per interim analysis
      alphaNE <- sE[n,] + alpha0E
      alphaNC <- sC[n,] + alpha0C
      
      # Sample from posterior distribution of phi
      postE <- SampleDir(nDraw, alphaNE)
      postC <- SampleDir(nDraw, alphaNC)
      
      # Compute treatment difference
      delta <- postE-postC
      
      # Compute posterior probability per rule
      PoP[rule,int] <- Rules[[rule]](delta)
      
      # Compute average treatment differences (delta, phi) per distribution
      MeanDelta[rule,int,] <- colMeans(cbind(rowSums(delta[,c(1,2)]), rowSums(delta[,c(1,3)])))
        }
      }
      
      if(type %in% c("BAD")) {
          # Sample size
          n <- as.vector(nInt[1,int])
          
          # Compute posterior alpha per interim analysis
          alphaNE <- sE[n,] + alpha0E
          alphaNC <- sC[n,] + alpha0C
          
          # Sample from posterior distribution of phi
          postE <- SampleDir(nDraw, alphaNE)
          postC <- SampleDir(nDraw, alphaNC)
          
          # Compute treatment difference
          delta <- postE-postC
          
          for(rule in 1:length(Rules)){
          # Compute posterior probability per rule
          PoP[rule,int] <- Rules[[rule]](delta)
          
          # Compute average treatment differences (delta, phi) per distribution
          MeanDelta[rule,int,] <- colMeans(cbind(rowSums(delta[,c(1,2)]), rowSums(delta[,c(1,3)])))
        }
      }
    }
  
  return(list(PoP=PoP,
              MeanDelta=MeanDelta))
}


#### 3. Optimize weights ####
# Function to find efficiency weights for the compensatory decision rule given K=2 (correlated) binary outcome variables.
# Input: phiE: Vector of length Q=2^K containing joint response probabilities phi of experimental treatment
#        phiC: Vector of length Q=2^K containing joint response probabilities phi of control treatment
#        n   : Sample size at which to approximate treatment difference assuming an underlying normal distribution
# Output: weights: Vector of length K with optimal weights for the compensatory decision rule

OptimizeWeights <- function(phiE, phiC, n){
  
  # Compute joint response frequencies
  SE <- phiE * n
  SC <- phiC * n
  
  # Obtain sample posterior distribution joint responses phi
  postE <- SampleDir(1e5, SE)
  postC <- SampleDir(1e5, SC)
  
  # Transform to sample treatment differences joint (marginal) probabilities delta
  delta <- cbind(rowSums(postE[,c(1,2)]) - rowSums(postC[,c(1,2)]), 
                 rowSums(postE[,c(1,3)]) - rowSums(postC[,c(1,3)]))
  
  # Find means and variances treatment difference delta
  delta_mean <- colMeans(delta)
  delta_var <- cov(delta)
  
  # Express standardized linear combination as a function of weights --> for optimization
  max.weights <- function(param, mu, sigma){
    w1 <- param[1]/sum(param)
    w2 <- param[2]/sum(param)
    fit <- (0-(w1 * mu[1] + w2 * mu[2]))/
      sqrt(w1^2 * sigma[1,1] + w2^2 * sigma[2,2] + 2 * w1 * w2 * sigma[2,1])
    return(fit)
  }
  
  # Optimize
  res <- optim(par=c(0.5,0.5),fn=max.weights, mu=delta_mean, sigma=delta_var)
  weights <- res$par/sum(res$par)      
  
  return(weights)
} 

#### 4. Store results as a workspace ####
# Function to store workspace.

# Input:  x:          Object of storage
#         file:       Directory and filename of stored object

AppendResults                        <- function(x, file){
  if(file.exists(file)){
    old.objects                      <- load(file)
    save(list=c(old.objects, deparse(substitute(x))), file=file)                            
  }else{save(list=deparse(substitute(x)), file=file)}}
