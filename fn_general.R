

# Functions ---------------------------------------------------------------

# Function to display two input boxes side-by-side
numericInputRow<-function(inputId, label, value, min, max) 
{
  div(style="display:inline-block",
        tags$head(
        tags$style(HTML("
         input[type=\"number\"] {
          width: 200px;
         }
       "))
       ),
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, 
                 type = "number", 
                 value = value,
                 min = min, 
                 max = max
               #  class="input-small"
                 )
      )
}


# Function to calculate sensitivity and specificity for each study
study_level_outcomes <- function(data = NULL, subset=NULL, formula = NULL,
                                 TP="TP", FN="FN", FP="FP", TN="TN")
{
  if (!is.null(data) & is.character(c(TP,FP,TN,FN))) {
    X <- as.data.frame(data)
    origdata <- data
    TP <- getElement(X,TP)
    FN <- getElement(X,FN) 
    FP <- getElement(X,FP)
    TN <- getElement(X,TN)
  }
  if (is.null(data) & !is.character(c(TP,FP,TN,FN))) {
    origdata <- data.frame(TP = TP, FN = FN, FP = FP, TN = TN)
  }
  
  freqdata <- cbind(TP,FN,FP,TN)
  # Need checkdata function (see below)
  checkdata(freqdata)
  
  # Sensitivity and specificity calculations for each study
  origdata$sens <- origdata$TP / (origdata$TP + origdata$FN)
  origdata$spec <- origdata$TN / (origdata$FP + origdata$TN)
  origdata$fpr <- 1- origdata$spec
  
  study_level <- data.frame(TP=origdata$TP, FN=origdata$FN, FP=origdata$FP, TN=origdata$TN, 
                            N=(origdata$TP+origdata$FN+origdata$FP+origdata$TN), 
                            Sensitivity=origdata$sens, Specificity=origdata$spec, FPR=origdata$fpr)
  # Run the Garabage Collector to Ensure any excess memory used by stan is freed
  gc()
  return(study_level)
}

# Function needed to checkdata in correct format before calculating sens and spec for each study
checkdata <- function(X, nrowwarn = 5) {
  X <- as.data.frame(X)
  if(!all(c("TP","FN","FP","TN") %in% names(X))){
    stop("Data frame or matrix must have columns labelled TP, FN, FP and TN.")}
  if(!identical(round(X),as.data.frame(apply(X,2,as.numeric)))){
    warning("Some of the values of TP,FN,FP or TN have non-zero decimal places. Did you forget to round?")}
  if(nrow(X) < nrowwarn){warning("There are very few primary studies!")}
  idx_too_many_zeroes <- apply(X,1,function(x){sum(x == 0)}) > 2
  if(any(idx_too_many_zeroes)){
    stop(paste("Some study with three or more zeroes in 2x2 table! Row:", which(idx_too_many_zeroes)))
  }
  return(invisible(NULL))
}

# functions for cts/cat covariates 


# function that extracts the user-selected covariate (cov_index) and transforms to integers (for categorical / discrete ) covariates,
# e.g. is covariate is 1.1,1.2,1.2,1.4 for a dataset with 4 studies then function outputs 1,2,2,3
if_else_Cov <- function(X, cov_index) {
  # Count the number of columns
  C <- ncol(dplyr::select(X, -year.cts, -prevalence.cts))
  # Store the names of the columns
  Names <- colnames(dplyr::select(X, -year.cts, -prevalence.cts))
  num_levels <- NULL
  cts_cov_points <- NULL
  if (C > 6 & Names[7] != "rob_PS") { # cov.csv
    if (str_sub(colnames(X)[6 + cov_index], start = -3) == "cts") {
      out <- as.numeric(X[, 6 + cov_index])
      cts_cov_points <- seq(from = min(as.numeric(X[, 6 + cov_index])), 
                            to = max(as.numeric(X[, 6 + cov_index])), length = 101)
    } else {
      out <- as.numeric(factor(X[, 6 + cov_index]))
      num_levels <- length(unique(X[, 6 + cov_index]))
    }
  } else { # QA_cov.csv
    if (str_sub(colnames(X)[13 + cov_index], start = -3) == "cts") {
      out <- as.numeric(X[, 13 + cov_index])
      cts_cov_points <- seq(from = min(as.numeric(X[, 13 + cov_index])), 
                            to = max(as.numeric(X[, 13 + cov_index])), length = 101)
    } else {
      out <- as.numeric(factor(X[, 13 + cov_index]))
      num_levels <- length(unique(X[, 13 + cov_index]))
    }
  }
  my_list <- list("out" = out, "num_levels" = num_levels, "cts_cov_points" = cts_cov_points)
  # Run the Garabage Collector to Ensure any excess memory used by stan is freed
  gc()
  return(my_list)
}

# function that extracts the user-selected covariate (cov_index) and transforms to the levels of the (for categorical / discrete ) covariates,
# e.g. is covariate is 1.1,1.2,1.2,1.4 for a dataset with 4 studies then function outputs 1,2,3. 
if_else_Cov_level <- function(X, cov_index) {
  # Count the number of columns
  C <- ncol(dplyr::select(X, -year.cts, -prevalence.cts))
  # Store the names of the columns
  Names <- colnames(dplyr::select(X, -year.cts, -prevalence.cts))
  if (C > 6 & Names[7] != "rob_PS") { # cov.csv
    if (str_sub(colnames(X)[j + cov_index], start = -3) == "cts") {
      out <- sort(unique(as.numeric(X[, j + cov_index])))
    } else {
      out <- sort(unique(as.numeric(factor(X[, j + cov_index]))))
    }
  } else { # QA_Cov.csv
    if (str_sub(colnames(X)[j + cov_index], start = -3) == "cts") {
      out <- sort(unique(as.numeric(X[, 13 + cov_index])))
    } else {
      out <- sort(unique(as.numeric(factor(X[, 13 + cov_index]))))
    }
  }
  # Run the Garabage Collector to Ensure any excess memory used by stan is freed
  gc()
  return(out)
}


## Function to calculate the number of covariates (for Cov.csv and QA_Cov.csv datasets)  -------
num_covariates <- function(X) { # input X is a data frame
  # Count the number of columns
  C <- ncol(dplyr::select(X, -year.cts, -prevalence.cts))
  # Store the names of the columns
  Names <- colnames(dplyr::select(X, -year.cts, -prevalence.cts))
  initial <- c("None")
  if ( (C > 6 & Names[7] != "rob_PS") | (C > 13 & Names[13] == "ac_RS") ) { # either cov.csv or QA_cov.csv
    if (C > 6 & Names[7] != "rob_PS") { # cov.csv
      if (C == 7) {  # one covariate
        covariates <- colnames(X[7])
      }
      else { # > 1 covariate
        covariates <- colnames(X[,7:C])
      }
    }
    if (C > 13 & Names[13] == "ac_RS") { # QA_cov.csv
      if (C == 14) {
        covariates <- colnames(X[14])
      }
      else {
        covariates <- colnames(X[,14:C])
      }
    }
    combined <- c(initial, covariates)
    number <- 1:length(combined)
    choicesCov <- setNames(number, combined)
    no_covariates <- length(number) - 1
    # put objects in a list - so function can return multiple outputs
    my_list <- list("combined" = combined, 
                    "number" = number,
                    "choicesCov" = choicesCov,
                    "no_covariates" = no_covariates)
  }
  else { # if using a dataset w/ no covariates (i.e. Standard.csv or QA.csv)
    combined <- initial
    number <- 1
    no_covariates <- 0
    choicesCov <- setNames(number, combined)
    no_covariates <- length(number) - 1
    # put objects in a list - so function can return multiple outputs
    my_list <- list("combined" = combined, 
                    "number" = number,
                    "choicesCov" = choicesCov,
                    "no_covariates" = no_covariates)
  }
  # Run the Garabage Collector to Ensure any excess memory used by stan is freed
  gc()
  return(my_list)
}



# ## Function to extract observed values from each study ("ss") dataset ----------------------------------------------------------------------
obs_values <- function(X) { 
  ss<- tibble( 
    Study =as.numeric(as.factor(X$author)), 
    TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
    N=(X$TP+X$FN+X$FP+X$TN) ,
    Sensitivity= (TP/(TP+FN))  , 
    Specificity= (TN/(TN+FP))  , 
    FPR = 1 - Specificity,
    prev = round((TP+FN)/N, 2)
  )
  n_individuals <- sum(X$TP+X$FN+X$FP+X$TN)
  n_studies <- nrow(X)
  my_list <- list("ss" = ss, 
                  "n_studies" = n_studies, 
                  "n_individuals" = "n_studies")
  # Run the Garabage Collector to Ensure any excess memory used by stan is freed
  gc()
  return(my_list)
}


# Function to extract dataset containing only Author, year, sens,  --------

data_summary_for_display <- function(X) { 
  # observed values dataset
  ss <- obs_values(X)$ss
  N <- obs_values(X)$n_studies # number of studies
  ss2 <- left_join(ss, X)
  non_covariate_info <- dplyr::select(ss2, author, year, Sensitivity, Specificity, FPR)
  
  no_covariates <- num_covariates(X)$no_covariates # number of covariates
  cov_names <-  num_covariates(X)$combined[-1] # extract names of covariates
  cols_covariates <- dplyr::select(ss2, cov_names, year.cts, prevalence.cts) # columns containing only covariate data
  
  if (no_covariates > 0) { # if using a dataset with covariates 
    ss3 <- cbind(non_covariate_info, cols_covariates)
    cols <- c("author", "year", "Sensitivity", "Specificity", "FPR",  num_covariates(X)$combined[-1])
  } 
  else { # if using a dataset with no covariates (ie. Standard.csv or QA.csv)
    ss3 <- non_covariate_info
    cols <- c("author", "year", "Sensitivity", "Specificity", "FPR")
  }
  
  data <- dplyr::select(ss3, cols)
  # Run the Garabage Collector to Ensure any excess memory used by stan is freed
  gc()
  return(data)
}





#### functions to run k-fold cross validation, found on GitHub from:
#### https://github.com/stan-dev/stancon_talks/blob/master/2017/Contributed-Talks/07_nicenboim/kfold.Rmd

# The following function can run all the chains of all the folds of the model in parallel:
stan_kfold <- function(file, list_of_datas, chains, cores,...){
  badRhat <- 1.1
  K <- length(list_of_datas)
  model <- stan_model(file=file)
  # First parallelize all chains:
  sflist <- 
    pbmclapply(1:(K*chains), mc.cores = cores, 
               function(i){
                 # Fold number:
                 k <- round((i+1) / chains)
                 s <- sampling(model, data = list_of_datas[[k]], 
                               chains = 1, chain_id = i,  ...)
                 # Run the Garabage Collector to Ensure any excess memory used by stan is freed
                 gc()
                 return(s)
               })
  # Then merge the K * chains to create K stanfits:
  stanfit <- list()
  for(k in 1:K){
    inchains <- (chains*k - 2):(chains*k)
    # Merge `chains` of each fold
    stanfit[[k]] <- sflist2stanfit(sflist[inchains])
  }  
  # Run the Garabage Collector to Ensure any excess memory used by stan is freed
  gc()
  return(stanfit) 
}

# Wrapper function to extract the log_lik of the held-out data, given a list of stanfits, and a list which indicates with 1 and 0 whether the observation was held out or not:
extract_log_lik_K <- function(list_of_stanfits, list_of_holdout, ...){
  K <- length(list_of_stanfits)
  list_of_log_liks <- plyr::llply(1:K, function(k){
    extract_log_lik(list_of_stanfits[[k]], merge_chains = TRUE , ...)
  })
  # `log_lik_heldout` will include the loglike of all the held out data of all the folds.
  # We define `log_lik_heldout` as a (samples x N_obs) matrix
  # (similar to each log_lik matrix)
  log_lik_heldout <- list_of_log_liks[[1]] * NA
  for(k in 1:K){
    log_lik <- list_of_log_liks[[k]]
    samples <- dim(log_lik)[1] 
    N_obs <- dim(log_lik)[2]
    # This is a matrix with the same size as log_lik_heldout
    # with 1 if the data was held out in the fold k
    heldout <- matrix(rep(list_of_holdout[[k]], each = samples), nrow = samples)
    # Sanity check that the previous log_lik is not being overwritten:
    if(any(!is.na(log_lik_heldout[heldout==1]))){
      warning("Heldout log_lik has been overwritten!!!!")
    }
    # We save here the log_lik of the fold k in the matrix:
    log_lik_heldout[heldout==1] <- log_lik[heldout==1]
  }
  # Run the Garabage Collector to Ensure any excess memory used by stan is freed
  gc()
  return(log_lik_heldout)
}


kfold <- function(log_lik_heldout)  {
  library(matrixStats)
  logColMeansExp <- function(x) {
    # should be more stable than log(colMeans(exp(x)))
    S <- nrow(x)
    colLogSumExps(x) - log(S)
  }
  # See equation (20) of @VehtariEtAl2016
  pointwise <-  matrix(logColMeansExp(log_lik_heldout), ncol= 1)
  colnames(pointwise) <- "elpd"
  # See equation (21) of @VehtariEtAl2016
  elpd_kfold <- sum(pointwise)
  se_elpd_kfold <-  sqrt(ncol(log_lik_heldout) * var(pointwise))
  
  out <- list(
    pointwise = pointwise,
    elpd_kfold = elpd_kfold,
    se_elpd_kfold = se_elpd_kfold)
  out
  #structure(out, class = "loo")
}


