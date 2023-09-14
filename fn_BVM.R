

# Functions to extract study weights from Stan model output  -----------------------------------------------------------------------
# for MA

MA_weights <- function(X, mod) { 
  
        N <- nrow(X)
        
        Z <- matrix(data = c(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Z"))$summary[,5]), nrow = 2*N, ncol = 2*N)
        G_one <- matrix(data = c(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("G_one"))$summary[,5]), nrow = 2)
        BI <- matrix(data = c(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("BI"))$summary[,5]), nrow = 2*N, ncol = 2*N)
        d <- c(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("d"))$summary[,5])
        nd <- c(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("nd"))$summary[,5])
        
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
        
        G <- do.call(adiag, replicate(N, G_one, simplify = FALSE))
        V <- (Z %*% G %*% t(Z)) + (A %*% BI)
        # invert the variance matrix
        invV <- solve(V)
        # derive the fishers information matrix
        fish <- XT_pw %*% invV %*% X_pw
        # invert Fishers information to obtain Var Beta hat
        varb <- solve(fish)
        pctse <- vector(mode="numeric", length = N)
        pctsp <- vector(mode="numeric", length = N)
        for (i in 1:N) {
          DM <- V
          DM[(i*2)-1, (i*2)-1] <- 1000000000
          DM[(i*2)-1, (i*2)] <- 0
          DM[(i*2), (i*2)-1] <- 0
          DM[(i*2), (i*2)] <- 1000000000
          invDM <- solve(DM)
          fishD <- XT_pw %*% invDM %*% X_pw
          fishI <- fish - fishD
          weight <- varb %*% fishI %*% varb
          pctse[i] <- 100*(weight[1,1]/varb[1,1])
          pctsp[i] <- 100*(weight[2,2]/varb[2,2])
        }
        
        X2 <- X %>%  dplyr::mutate(pctse = pctse, pctsp = pctsp)
        # Run the Garabage Collector to Ensure any excess memory used by stan is freed
        gc()
        return(X2)
        
}







MA_cred_pred_roc <- function(X, 
                             mod) { 

          # Add study weights to dataset
          X <- MA_weights(X, mod)
          n_studies <- obs_values(X)$n_studies 
      
      ## BVM sROC plot - credible region  -------------------------------------------------------------------------
          cred <- tibble(y = (rstan::extract(mod, pars = "lSe")$lSe) , x = (rstan::extract(mod, pars = "lSp")$lSp))
          # in inv_probit space
          g <- ggplot(data = cred, aes(x = x, y = y))  + 
            stat_ellipse()  
          # Get ellipse coordinates from plot
          pb <-  ggplot_build(g)
          el = pb$data[[1]][c("x","y", "group")]
          credible_region <- tibble(x = plogis(el$x), y = plogis(el$y))
          
          g <- ggplot(data = credible_region, aes(x = x, y = y))  + 
            geom_polygon(data = credible_region, aes(x = 1  - x, y = y), alpha=0.05, size=0.4)  + 
            xlim(0,1) + 
            ylim(0,1)
      
      ## BVM sROC plot - prediction region  -------------------------------------------------------------------------
          pred <- tibble(y = (rstan::extract(mod, pars = "lSe_pred")$lSe_pred), x = (rstan::extract(mod, pars = "lSp_pred")$lSp_pred))
          # in inv_probit space
          g <- ggplot(data = pred, aes(x = x, y = y))  + 
            stat_ellipse()  
          
          # Get ellipse coordinates from plot
          pb <-  ggplot_build(g)
          el = pb$data[[1]][c("x","y", "group")]
          pred_region <- tibble(x = plogis(el$x), y = plogis(el$y))
      
      ## BVM sROC plot - summary estimates (posterior medians)  -----------------------------------------------------
          median_sens <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
          median_spec <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary[,5])
          medians <- tibble(median_sens = median_sens, median_spec = median_spec)
          

      ## BVM sROC plot - HSROC curve   ------------------------------------------------------------------------------
          TPR <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("roc_points_tpr"))$summary[,5])
          FPR <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("roc_points_fpr"))$summary[,5])
          
          # Calculate min and max values of sens and spec to avoid extrapolation of SROC curve
          minTPR <- min(  X$TP/(X$TP+X$FN) )
          maxTPR <- max(  X$TP/(X$TP+X$FN) )
          minFPR <- min(  X$FP/(X$FP+X$TN) )
          maxFPR <- max(  X$FP/(X$FP+X$TN) )
          
          roc_points <- tibble(TPR, FPR)
          
          # Create new data frame which restricts roc_points to being between min and max values
          roc_points_restricted <- subset(roc_points, 
                                          FPR<maxFPR & FPR>minFPR & 
                                          TPR<maxTPR & TPR>minTPR)
          
          # unrestricted HSROC curve 
          roc_points_unrestricted <- roc_points
          
      ## BVM sROC plot - study-specific (observed) data points and CI's  ---------------------------------------------
      
          # observed values
          ss <- obs_values(X)$ss %>% 
                dplyr::mutate(pctse = X$pctse, pctsp = X$pctsp)
          
          # Calculate sens and spec confidence intervals at the study level
          foreach (i = 1:n_studies) %do% {
            ss$Sens_LCI[i] <- binconf(ss$TP[i], ss$TP[i]+ss$FN[i], method="exact")[2]
            ss$Sens_UCI[i] <- binconf(ss$TP[i], ss$TP[i]+ss$FN[i], method="exact")[3]
            ss$FPR_LCI[i]  <- 1 - binconf(ss$TN[i], ss$FP[i]+ss$TN[i], method="exact")[3]
            ss$FPR_UCI[i]  <- 1 - binconf(ss$TN[i], ss$FP[i]+ss$TN[i], method="exact")[2]
          }
      
      my_list <- list("ss" = ss, 
                      "X" = X, 
                      # "roc_points2" = roc_points2,
                      "roc_points_restricted" = roc_points_restricted,
                      "roc_points_unrestricted" = roc_points_unrestricted,
                      "pred_region" = pred_region, 
                      "credible_region" = credible_region,
                      "medians" = medians)
      # Run the Garabage Collector to Ensure any excess memory used by stan is freed                
      gc()
      return(my_list)


}



