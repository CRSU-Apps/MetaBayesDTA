


# Functions to extract study weights from Stan model output  -----------------------------------------------------------------------------------------------------------------------
# for MA


LCM_cred_pred_roc <- function(X,
                              mod, 
                              index_test) { 

                    params <- rstan::extract(mod)
                    
                    num_refs <- ncol(params$Se_ref)
                    refs_names_short <- c(levels(factor(X$reference.cat)))
                    refs_names <- c(paste0("Ref","- ",refs_names_short))

                    
                    Sens_refs <- list()
                    Spec_refs <- list()
                    
                    for (i in 1:num_refs) { 
                      Sens_refs[[i]] <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se_ref"))$summary[i,]
                      Spec_refs[[i]] <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp_ref"))$summary[i,]
                    }
                    
                    correlation_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Omega_ref"))$summary
                    between_study_sd_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_ref"))$summary
                    
                    Sens_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se_index"))$summary
                    Spec_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp_index"))$summary
                    correlation_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Omega_index"))$summary
                    between_study_sd_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_index"))$summary
                    
                    prev <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("p"))$summary
                    
             ## LCM sROC plot - credible regions ----------------------------------------------------------------------
                    cred_ref <- list()
                    g_ref <- list()
                    pb_ref <- list()
                    el_ref <- list()
                    credible_region_ref <- list()
                    
                    # for reference tests
                    for (i in 1:num_refs) { 
                      cred_ref[[i]] <- tibble(y = (rstan::extract(mod, pars = "ref_logit_mu")$ref_logit_mu[,i,1]) , 
                                              x = (rstan::extract(mod, pars = "ref_logit_mu")$ref_logit_mu[,i,2]))
                      # in inv_probit space
                      g_ref[[i]] <- ggplot(data = cred_ref[[i]], aes(x = x, y = y))  + 
                        stat_ellipse()
                      
                      # Get ellipse coordinates from plot
                      pb_ref[[i]] <-  ggplot_build(g_ref[[i]])
                      el_ref[[i]] = pb_ref[[i]]$data[[1]][c("x","y", "group")]
                      credible_region_ref[[i]] <- tibble(x = plogis(el_ref[[i]]$x), y = plogis(el_ref[[i]]$y))
                    }
                    
                    refs_names_numbers <- tibble(ref_names = refs_names, .id = as.numeric(as.factor(refs_names)))
                    
                    credible_region_ref2 <- tibble(data.table::rbindlist(credible_region_ref, idcol = TRUE))
                    
                    credible_region_ref3 <-   dplyr::left_join(credible_region_ref2, refs_names_numbers) %>% 
                                              dplyr::select(ref_names, x, y) %>%
                                              dplyr::rename(Test = ref_names)
                    
                    
                    # for index test
                    cred_index <- tibble(y = (rstan::extract(mod, pars = "index_logit_mu")$index_logit_mu[,1]) , 
                                         x = (rstan::extract(mod, pars = "index_logit_mu")$index_logit_mu[,2]))
                    g_index <- ggplot(data = cred_index, aes(x = x, y = y))  + 
                      stat_ellipse()
                    pb_index <-  ggplot_build(g_index)
                    el_index = pb_index$data[[1]][c("x","y", "group")]
                  
                    credible_region_index <- tibble(x = plogis(el_index$x), y = plogis(el_index$y), Test =  index_test)
                    
                    # merge datasets for regions for ref tests and index test into one dataset
                    credible_region <- rbind(credible_region_index, credible_region_ref3) %>%
                      dplyr::mutate(Test = factor(Test, levels =  c(refs_names, index_test) ))
                    
            ## LCM sROC plot - prediction regions ----------------------------------------------------------------------
                    ## prediction region for index test
                    pred_index <- tibble(y = (rstan::extract(mod, pars = "lSe_pred_index")$lSe_pred_index), 
                                         x = (rstan::extract(mod, pars = "lSp_pred_index")$lSp_pred_index))
                    # in inv_probit space
                    g_index <- ggplot(data = pred_index, aes(x = x, y = y))  + 
                      stat_ellipse()  
                    # Get ellipse coordinates from plot
                    pb_index <-  ggplot_build(g_index)
                    el_index = pb_index$data[[1]][c("x","y", "group")]
                    
                    pred_region_index <- tibble(x = plogis(el_index$x), y = plogis(el_index$y),  Test =  index_test)
                    
                    
                    ## prediction region for reference tests
                    pred_ref <-list()
                    g_ref <- list()
                    pb_ref <- list()
                    el_ref <- list()
                    pred_region_ref <- list()
                    
                    for (i in 1:num_refs) { 
                      pred_ref[[i]]  <- tibble(y = (rstan::extract(mod, pars = "lSe_pred_ref")$lSe_pred_ref[, i]), 
                                               x = (rstan::extract(mod, pars = "lSp_pred_ref")$lSp_pred_ref[, i]))
                      
                      # in inv_probit space
                      g_ref[[i]]  <- ggplot(data = pred_ref[[i]] , aes(x = x, y = y))  + 
                        stat_ellipse()  
                      
                      # Get ellipse coordinates from plot
                      pb_ref[[i]]  <-  ggplot_build(g_ref[[i]] )
                      el_ref[[i]]  = pb_ref[[i]] $data[[1]][c("x","y", "group")]
                      
                      pred_region_ref[[i]]  <- tibble(x = plogis(el_ref[[i]]$x), y = plogis(el_ref[[i]]$y))
                    }
                    
                    # unlist and put into a data frame
                    refs_names_numbers <- tibble(ref_names = refs_names, .id = as.numeric(as.factor(refs_names)))
                    
                    pred_region_ref2 <- tibble(data.table::rbindlist(pred_region_ref, idcol = TRUE))
                    
                    pred_region_ref3 <-   dplyr::left_join(pred_region_ref2, refs_names_numbers) %>% 
                                          dplyr::select(ref_names, x, y) %>%
                                          dplyr::rename(Test = ref_names)
                    
                    # merge datasets for regions for ref tests and index test into one dataset
                    pred_region     <- rbind(pred_region_index, pred_region_ref3) %>% 
                      dplyr::mutate(Test = factor(Test, levels =  c(refs_names, index_test) ))
                    
                    
                    
             ## LCM sROC plot - summary estimates  ----------------------------------------------------------------------
                    ## medians
                    median_sens <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
                    median_spec <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary[,5])

                    
                    medians <- tibble(median_sens = median_sens,
                                      median_spec = median_spec,
                                      Test = c(refs_names, index_test))
                    
                    
             ## LCM sROC plot - model-estimated study-specific points  ----------------------------------------------------------------------
                    
                    
                    se_ref <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_ref"))$summary[,5])
                    sp_ref <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_ref"))$summary[,5])
                    se_index <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_index"))$summary[,5])
                    sp_index <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_index"))$summary[,5])
                    prevs <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("p"))$summary[,5])
                    
                    se_ref_LCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_ref"))$summary[,4])
                    se_ref_UCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_ref"))$summary[,6])
                    sp_ref_LCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_ref"))$summary[,4])
                    sp_ref_UCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_ref"))$summary[,6])
                    
                    se_index_LCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_index"))$summary[,4])
                    se_index_UCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_index"))$summary[,6])
                    sp_index_LCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_index"))$summary[,4])
                    sp_index_UCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_index"))$summary[,6])
                    
                    ss<- tibble( 
                      Study =as.numeric(as.factor(X$author)), 
                      TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
                      N=(X$TP+X$FN+X$FP+X$TN) ,
                      se_ref = se_ref   , 
                      se_ref_LCI = se_ref_LCI,
                      se_ref_UCI = se_ref_UCI,
                      sp_ref = sp_ref  , 
                      sp_ref_LCI = sp_ref_LCI,
                      sp_ref_UCI = sp_ref_UCI,
                      se_index = se_index   , 
                      se_index_LCI = se_index_LCI,
                      se_index_UCI = se_index_UCI,
                      sp_index = sp_index  , 
                      sp_index_LCI = sp_index_LCI,
                      sp_index_UCI = sp_index_UCI,
                      obs_prev = round((TP+FN)/N, 2), 
                      est_prev = round(prevs, 2)
                    )
                    
                    ss <- ss  %>%
                          dplyr::mutate(Test = paste0("Ref", "- ", X$reference.cat))
                    

              ## LCM sROC plot - HSROC curves  ----------------------------------------------------------------------
                    
                # for index  ---------------------------------------------------
                TPR_index <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("roc_points_tpr_index"))$summary[,5])
                FPR_index <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("roc_points_fpr_index"))$summary[,5])
                
                # Calculate min and max values of sens and spec to avoid extrapolation of SROC curve
                minTPR_index <- min( ss$se_index )
                maxTPR_index <- max( ss$se_index )
                minFPR_index <- min(  1 - ss$sp_index )
                maxFPR_index <- max(  1 - ss$sp_index )
                roc_points_index <- tibble(TPR_index, FPR_index)
                
                # Create new data frame which restricts roc_points to being between min and max values
                roc_points_index_restricted <- dplyr::filter(roc_points_index,
                                                               FPR_index < maxFPR_index & FPR_index > minFPR_index & 
                                                               TPR_index < maxTPR_index & TPR_index > minTPR_index) 
                # unrestricted HSROC curve
                roc_points_index_unrestricted <- roc_points_index
                
               
                
                # for ref tests -----------------------------------------------
                TPR_ref <- list()
                FPR_ref <- list()
                
                for (i in 1:num_refs) { 
                  TPR_ref[[i]] <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), 
                                                  pars = c("roc_points_tpr_ref"))$summary[,5][(101*(i-1) + 1):(101*i)])
                  FPR_ref[[i]] <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), 
                                                  pars = c("roc_points_fpr_ref"))$summary[,5][(101*(i-1) + 1):(101*i)])
                }
                
                # need to subset the dataset to only contain obs. from each reference test  
                minTPR_ref <- c()
                maxTPR_ref <- c()
                minFPR_ref <- c()
                maxFPR_ref <- c()
                roc_points_refs  <- list()
                roc_points_refs2 <- list()
                
                for (i in 1:num_refs) { 
                  
                  roc_points_refs[[i]] <- tibble(TPR_ref[[i]], FPR_ref[[i]])

                  # subset data to only contain obs. from Ref test i
                  ss_sub <- dplyr::filter(ss, 
                                          Test == paste0("Ref", "- ", refs_names_short[i]))

                  # subset based on model-estimated data 
                  # (NOT observed data like the standard BVM since there is no observed 
                  # study-specific accuracy data when not assuming a perfect GS!)
                  
                  minTPR_ref[i] <- min(ss_sub$se_ref) ; minTPR_ref[i] 
                  maxTPR_ref[i] <- max(ss_sub$se_ref) ; maxTPR_ref[i] 
                  
                  minFPR_ref[i] <- min(1 - ss_sub$sp_ref ) ; minFPR_ref[i]
                  maxFPR_ref[i] <- max(1 - ss_sub$sp_ref ) ; maxFPR_ref[i] 
                  
                  # Create new data frame which restricts roc_points to being between min and max values
                  roc_points_refs2[[i]] <- dplyr::filter(roc_points_refs[[i]], 
                                                         FPR_ref[[i]] > minFPR_ref[i] & FPR_ref[[i]] < maxFPR_ref[[i]] & 
                                                         TPR_ref[[i]] > minTPR_ref[i] & TPR_ref[[i]] < maxTPR_ref[[i]])
                  
                }
                
                
                
                roc_points_refs3   <- tibble(data.table::rbindlist(roc_points_refs2, 
                                                                   idcol = TRUE))
                
                refs_names_numbers <- tibble(ref_names = refs_names, .id = as.numeric(as.factor(refs_names)))
                
                roc_points_refs_restricted <-   dplyr::left_join(roc_points_refs3, refs_names_numbers) %>% 
                                                dplyr::select(ref_names, `TPR_ref[[i]]`, `FPR_ref[[i]]`) %>%
                                                dplyr::rename(Test = ref_names , TPR = `TPR_ref[[i]]` ,  FPR = `FPR_ref[[i]]`)
                
                # unrestricted HSROC curve
                roc_points_refs_unrestricted   <- tibble(data.table::rbindlist(roc_points_refs,  idcol = TRUE)) 
                
                
                roc_points_refs_unrestricted    <- dplyr::left_join(roc_points_refs_unrestricted, refs_names_numbers) %>% 
                                                   dplyr::select(ref_names, `TPR_ref[[i]]`, `FPR_ref[[i]]`) %>%
                                                   dplyr::rename(Test = ref_names , TPR = `TPR_ref[[i]]` ,  FPR = `FPR_ref[[i]]`)
                
                

                

                    

                    
my_list <- list("ss" = ss, 
                "X" = X, 
               # "roc_points_refs4" = roc_points_refs4,
                "roc_points_refs_restricted" = roc_points_refs_restricted,
                "roc_points_refs_unrestricted" = roc_points_refs_unrestricted,
                # "roc_points_index2" = roc_points_index2,
                "roc_points_index_restricted" = roc_points_index_restricted,
                "roc_points_index_unrestricted" = roc_points_index_unrestricted,
                "pred_region" = pred_region, 
                "credible_region" = credible_region,
                "medians" = medians,
                "num_refs" = num_refs,
                "refs_names_short" = refs_names_short,
                "refs_names" = refs_names)
# Run the Garabage Collector to Ensure any excess memory used by stan is freed
gc()
return(my_list)


}



