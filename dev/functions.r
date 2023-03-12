#' Function to load and divide date into subgroups of player per position
#' 
#' @param sName string, directory and name of the datafile
#' @return list of the dataframes for the whole dataset and subgroups
Data_Prepare <- function(sName){
  load(sName)
  dfData <- fifa
  
  # Get player positions
  dfDefense = dfData %>% filter(dfData$Position == 'Def')
  dfMidfield = dfData %>% filter(dfData$Position == 'Mid')
  dfAttack = dfData %>% filter(dfData$Position == 'FW')
  dfGoal = dfData %>% filter(dfData$Position == 'Gk')
  
  return(list(dfData,dfDefense,dfMidfield,dfAttack,dfGoal))
}

#' Function to drop non-numeric columns and scale the remaining columns
#' 
#' @param lDFs list of dataframes
#' @return lDFs list of processed dataframes
DropAndScale = function(lDFs){
  for (i in 1:length(lDFs)){
    lDFs[[i]] = lDFs[[i]][, !names(lDFs[[i]]) %in% c("name", "club", "Position", "eur_value", "eur_wage", "eur_release_clause")]
    lDFs[[i]] <- na.omit(lDFs[[i]])
    lDFs[[i]] = scale(lDFs[[i]])
  }
  return(lDFs)
}


#' Function to define the soft threshold
#' 
#' @param x column vector of p elements
#' @param lambda double, optimal penalty term
#' @return Soft threshold operator
soft_threshold <- function(x, lambda) {
  dNorm <- norm(sign(x) * pmax(abs(x) - lambda, 0), type = "2")
  if(dNorm != 0){
    sign(x) * pmax(abs(x) - lambda, 0) / dNorm
  }else{
    sign(x) * pmax(abs(x) - lambda, 0)
  }
}

#' Function to define the define the objective function
#' 
#' @param x column vector of p elements
#' @param lambda double, optimal penalty term
#' @return objective function
objective_func <- function(x, lambda) {
  # Example objective function: sum of squares error
  norm(soft_threshold(x, lambda), type = "1")
}

#' Function to perform binary search for optimal lambda
#' 
#' @param x column vector of p elements
#' @param dC double, threshold value
#' @return lambda_optimal, the optimal lambda
Binary_Search <- function(x, dC){
  # Define the range of possible lambda values
  lambda_range <- c(0, 10)
  # Define the desired objective function value
  obj_val <- dC
  # Perform binary search for lambda
  while (diff(lambda_range) > 1e-6) {
    # Get the midpoint of the current range
    lambda_mid <- mean(lambda_range)
    # Evaluate the objective function at the midpoint
    obj_mid <- objective_func(x, lambda_mid)
    # Update the range of lambda values based on the objective function value
    if (obj_mid >= obj_val) {
      lambda_range[1] <- lambda_mid
    } else {
      lambda_range[2] <- lambda_mid
    }
  }
  # Print the optimal lambda value
  lambda_optimal <- mean(lambda_range)
  return(lambda_optimal)
}

#' Function to perform Sparce PCA
#' 
#' @param dfData dataframe, characteristics of players
#' @param i_max integer, maximum number of iterations
#' @return mV, sparse matrix of principal components
Sparse_PCA <- function(dfData, i_max){
  mR <- as.matrix(dfData)
  iK <- ncol(mR)
  for(j in 1: iK){
    lSVD <- svd(mR)
    # dC1 <- sqrt(dim(dfData)[1])
    dC2 <- sqrt(dim(mR)[2])
    vD <- lSVD$d
    mU <- lSVD$u
    mV <- lSVD$v
    i <- 1
    while(i < i_max){
      i <- i + 1
      # dLambda1 <- Binary_Search(mR %*% mV[, j], dC1)
      # mU[, j] <- soft_threshold(mR %*% mV[, j], dLambda1)
      mU[, j] <- mR %*% mV[, j] / norm(mR %*% mV[, j], type = "2")
      dLambda2 <- Binary_Search(t(mR) %*% mU[, j], dC2)
      mV[, j] <- soft_threshold(t(mR) %*% mU[, j], dLambda2)
      print(i)
    }
    mR <- mR - vD[j] * mU[, j] %*% t(mV[, j])
    print(j)
  }
  return(mV)
}

#' Function to get the most influential variables from SPCA results
#' 
#' @param mV_sparse mV matrix from sparse PCA output
#' @param n_var integer, number of variables wanted
#' @return l_Var_selected, list of n_var most influential variables and their magnitude of influence
var_spca = function(mV_sparse, n_var){
  mV_sparse = mV_sparse[,1:3]^2
  l_Var = rowSums(mV_sparse[,1:3]) %>% as.matrix() %>% sqrt() # Get variances of each variables
  rownames(l_Var) = colnames(dfData) # Get name of each variables
  l_Var=l_Var[order(l_Var, decreasing = TRUE), ] %>% as.data.frame()
  l_Var_selected=head(l_Var, n_var)
  return(l_Var_selected)
}

#' Function to prepare input object for Sparse PCA scree plot
#' 
#' @param sparse_PCA object output from sparse PCA function
#' @return dfSPCAscree, dataframe consisting of percentages of variance explained by the 10 most important principle components
process_Scree = function(sparse_PCA){
  vSPCAcum = as.vector(sparse_PCA$prop.var.explained)
  for(i in 1:10){
    if(i==1){
      vSPCAscree <- c(vSPCAcum[i]) *100
    }else{
      vSPCAscree <- rbind(vSPCAscree, 100*(vSPCAcum[i] - vSPCAcum[i-1]))
    }
  }
  dfSPCAscree <- as.data.frame(cbind(vSPCAscree, as.factor(seq(1,length(vSPCAscree)))))
  return(dfSPCAscree)
}

#' Function to plot Scree plot manually
#' 
#' @param df dataframe consisting of percentages of variance explained by the 10 most important principle components
#' @return None, plot a scree plot.
plotScree = function(df){
  plot.new()
  ggplot(df, aes(x=V2,y=V1, group=1)) +
    geom_bar(stat="identity", fill= "steelblue", color = "steelblue", position = "stack") +
    theme_minimal() +
    geom_line() +
    geom_point(shape=19) +
    xlab("Dimensions") +
    ylab("Percentage of explained variances") +
    scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10"))
}



