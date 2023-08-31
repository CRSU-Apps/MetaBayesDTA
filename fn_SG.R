


# Functions for bivariate SG model ---------------------------------------------------------------


# Subgroup analysis - function to extract study weights from Stan model output  --------

SG_weights <- function(X, mod, cov_index) { 
  
  C <- ncol(dplyr::select(X, -year.cts, -prevalence.cts))
  Names <- colnames(dplyr::select(X, -year.cts, -prevalence.cts))
  
  if (C > 6 & Names[7] != "rob_PS") { j <<- 6 } else { j <<- 13  }
  
  X <- X %>% arrange(!!as.name(colnames(X)[j + cov_index]))
  
  if (cov_index == 0) {
    print("Please select a covariate")
  }
  
  if (cov_index != 0) {
    if (str_sub(colnames(X)[j + cov_index], start = -3) != "cts") {
      
      num_levels <- length(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
      
      # extract posterior medians of Z, G_one, A, BI from Stan model output
      N <- nrow(X)
      Z <- matrix(data = c(summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Z"))$summary[,5]), nrow = 2*N, ncol = 2*N)
      BI <- matrix(data = c(summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("BI"))$summary[,5]), nrow = 2*N, ncol = 2*N)
      d <- c(summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("d"))$summary[,5])
      nd <- c(summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("nd"))$summary[,5])
      
      G_one <- list()
      
      for (i in 1:num_levels) { 
        G_one[[i]] <- matrix(data =  c(rstan::summary(mod, probs = c(0.025,  0.5, 0.975),
                                                      pars = c("G_one"))$summary[,5])[(1+4*(i-1)):(4+4*(i-1))], nrow = 2)
      }
      
      inv_n <- c()
      
      for (n in 1:N) {
        inv_n[2*n] <- 1/d[n]
        inv_n[2*n - 1] <- 1/nd[n]
      }
      
      A <- diag(inv_n)
      N <- length(X$TP)
      X$n1 <- X$TP+X$FN # number of diseased in each study
      X$n0 <- X$FP+X$TN # number of non-diseased in each study
      X$true1 <- X$TP
      X$true0 <- X$TN
      X$study <- 1:N
      
      Y_pw = reshape(X, direction = "long", varying = list( c("n0" , "n1") , c( "true0","true1" ) ) ,
                     timevar = "spec" , times = c(1,0) , v.names = c("n","true") )
      Y_pw = Y_pw[order(Y_pw$id),]
      Y_pw$sens<- 1-Y_pw$spec
      X_pw <- cbind(Y_pw$sens, Y_pw$spec) # reduced design matrix for the fixed-effect parameters
      XT_pw <- t(X_pw)
      
      G <- list()
      N_grp <- c()
      
      
      for (i in 1:num_levels) { 
        N_grp[i] <- sum(  as.numeric(factor(X[, j + cov_index])) == unique(as.numeric(factor(X[, j + cov_index])))[i]  ) # number of subgroups
        G[[i]] <- do.call(adiag, replicate(N_grp[i], G_one[[i]], simplify = FALSE))
      }
      
      
      V <- list()
      invV <- list()
      fish <- list()
      varb <- list()
      pctse <- list()
      pctsp <- list()
      
      N_grp2 <- c(0, N_grp)
      N_cs <- 2*cumsum(N_grp2)
      
      for (i in 1:num_levels) { 
        V[[i]] <- ( Z[(N_cs[i]+1):N_cs[i+1], (N_cs[i]+1):N_cs[i+1]] %*% G[[i]] %*% t(Z[(N_cs[i]+1):N_cs[i+1], (N_cs[i]+1):N_cs[i+1]]) ) + (A[(N_cs[i]+1):N_cs[i+1], (N_cs[i]+1):N_cs[i+1]] %*% BI[(N_cs[i]+1):N_cs[i+1], (N_cs[i]+1):N_cs[i+1]])
        # invert the variance matrix
        invV[[i]]  <- solve(V[[i]] )
        # derive the fishers information matrix
        fish[[i]]  <- XT_pw[ ,(N_cs[i]+1):N_cs[i+1]] %*% invV[[i]] %*% X_pw[(N_cs[i]+1):N_cs[i+1], ]
        # invert Fishers information to obtain Var Beta hat
        varb[[i]] <- solve(fish[[i]] )
        pctse[[i]]  <- vector(mode="numeric", length = N_grp[i])
        pctsp[[i]]  <- vector(mode="numeric", length = N_grp[i])
      }
      
      DM <-  list()
      invDM <-  list()
      fishD <-  list()
      fishI <-  list()
      weight <- list()
      
      for (i in 1:num_levels) { 
        for (g in 1:N_grp[i]) {
          DM[[i]] <- V[[i]]
          DM[[i]][(g*2)-1, (g*2)-1] <- 1000000000
          DM[[i]][(g*2)-1, (g*2)] <- 0
          DM[[i]][(g*2), (g*2)-1] <- 0
          DM[[i]][(g*2), (g*2)] <- 1000000000
          invDM[[i]] <- solve(DM[[i]])
          fishD[[i]] <- XT_pw[ ,(N_cs[i]+1):N_cs[i+1]] %*% invDM[[i]] %*% X_pw[(N_cs[i]+1):N_cs[i+1], ] 
          fishI[[i]] <- fish[[i]] - fishD[[i]]
          weight[[i]] <- varb[[i]] %*% fishI[[i]] %*% varb[[i]]
          pctse[[i]][g] <- 100*(weight[[i]][1,1]/varb[[i]][1,1])
          pctsp[[i]][g] <- 100*(weight[[i]][2,2]/varb[[i]][2,2])
        }
      }
      if (C > 6 & Names[7] != "rob_PS") { j <<- 6 } else { j <<- 13  }
      
      for (i in 1:num_levels) { 
        pctse[[i]] <- data.frame(pctse[[i]])
        pctsp[[i]] <- data.frame(pctsp[[i]])
      }
      
      if (C > 6 & Names[7] != "rob_PS") { j <<- 6 } else { j <<- 13  }
      
      pctse <- rbindlist(pctse, idcol = TRUE) %>% dplyr::rename(pctse = pctse..i.., !!as.name(colnames(X)[j + cov_index]) := .id)
      pctsp <- rbindlist(pctsp, idcol = TRUE) %>% dplyr::rename(pctsp = pctsp..i.., !!as.name(colnames(X)[j + cov_index]) := .id)
      
      pctse <- pctse %>% dplyr::mutate(!!as.name(colnames(X)[j + cov_index]) := as.integer(!!as.name(colnames(X)[j + cov_index])))
      pctsp <- pctsp %>% dplyr::mutate(!!as.name(colnames(X)[j + cov_index]) := as.integer(!!as.name(colnames(X)[j + cov_index])))
      
      
      # make cat. variable factor again
      pctse2 <- pctse
      for (i in 1:num_levels) {
        pctse2[, 1] = case_when(pctse[, 1] == i 
                                ~ levels(factor(X[, j + cov_index]))[i] , 
                                TRUE ~ as.character(c(pctse2[, 1])[[1]])  )
      }
      
      pctsp2 <- pctsp
      for (i in 1:num_levels) {
        pctsp2[, 1] = case_when(pctsp[, 1] == i 
                                ~ levels(factor(X[, j + cov_index]))[i] , 
                                TRUE ~ as.character(c(pctsp2[, 1])[[1]])  )
      }
      
      # add % study weights to the dataset
      X2 <- X %>%  dplyr::mutate(pctse = pctse2[,2], pctsp = pctsp2[,2])
      
    }
    
  }
  # Run the Garabage Collector to Ensure any excess memory used by stan is freed
  gc()
  return(X2)
}



