

# sROC plot --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# UI for sROC plot (output the plot)
MR_sroc_plot_UI <- function(id)   {
  ns <- NS(id)   
  tagList(
    
    
    p("NOTE: The dotted line(s) represent the 95% prediction region from the bivariate model; 
       the greyed out area(s) represent the 95% credible region from the bivariate model"),
    
    
    
    uiOutput(outputId =  ns("my_tooltip")), 
    plotOutput(outputId =  ns("plot"),
               click =  ns("plot_click"))
    

    
  )
}




# UI function to output sROC plot settings menu
MR_sroc_plot_settings_menu_UI <- function(id, 
                                          data) {
  ns <- NS(id)  
  tagList(
            sliderInput(inputId = ns("plot_dimension_slider"),
                        label = "Change size of plot",
                        min = 1,
                        max = 2000,
                        value = 500,
                        ticks = FALSE), 
            sliderInput(inputId = ns("size_summary"),
                        label = "Size of summary estimates",
                        min = 0,
                        max = 20,
                        step = 0.5,
                        ticks = FALSE,
                        value = 5),
            sliderInput(inputId = ns("size_study_specific"),
                        label = "Size of study-specific estimates",
                        min = 0,
                        max = 20,
                        step = 0.5,
                        ticks = FALSE,
                        value = 3),
            checkboxGroupInput(inputId = ns("HSROCcheck"),
                               label = h4("Options"),
                               choices = list("Observed data Points"=1, 
                                              "sROC curve"=2,
                                              "set x and y-axis limits to (0,1)"=3),
                               selected = list(1,3)),
            uiOutput(ns("HSROC_curve_type_ui")),
            conditionalPanel(condition = 'output.cov_type_obj == 1',
                             ns = ns, 
                             awesomeCheckbox(inputId = ns("cts_as_fct"),
                                          "Display covariate as categorical",
                                           FALSE)),
            
            awesomeCheckbox(inputId = ns("prevcheck"),
                          "Display disease prevalence",
                          FALSE),
            awesomeCheckbox(inputId = ns("weightcheck"),
                          "Display percentage study weights",
                          FALSE),
            conditionalPanel(condition = 'input.weightcheck == 1',  # works
                             ns=ns,
                             sliderInput(inputId = ns("weight_scale"),
                                         label = "scale to display study weights",
                                         min = 1,
                                         max = 2000,
                                         value = 500)),
            checkboxGroupInput(inputId = ns("cicheck"),
                               label = "Display 95% study level confidence intervals for observed data",
                               choices = list("Sensitivity"=1, "Specificity"=2)),
            uiOutput(ns("covariate_display_ui")),
            # input$covcheck_display comes from the "covariate_display_ui" renderUI object. 
            conditionalPanel(condition = 'input.covcheck_display != 1', 
                             ns=ns,
                             uiOutput(ns("covariate_model_ui"))),
            # Download plot:
            h5("Download plot:"),
            numericInput(inputId =  ns("plot_width"), label=h5("Plot width"), value = 5),
            numericInput(inputId =  ns("plot_height"), label=h5("Plot height"), value = 5),
            numericInput(inputId =  ns("plot_dpi"), label=h5("Plot DPI"), value = 600),
            downloadButton(outputId = ns("plot_download"), label = "Download Plot")
  )
}




# Server function to generate and download sROC plot
MR_sroc_plot_server <- function(id, 
                                data,
                                cts_cov_indicator,
                                draws) {
      
  moduleServer(
    id,
    function(input, output, session) {
      
    plot_object <- reactive({
      
            validate(
              need(!(is.null(draws())), 
                   "Please run model to display plot")
            )
      
            req(data(), draws(), input$covcheck_model, cancelOutput = TRUE)
            
            mod <- draws()
           
            cov_index <-  as.integer(as.double(input$covcheck_model)) - 1

            X <- data()
            N <- nrow(X) # number of studies
            
            
            C <-  ncol(data())  
            Names <-  colnames(data()) 
            if (C > 8 & Names[7] != "rob_PS") {   j <<- 6  } else { j <<- 13  }
            
            
            # extract covariate info from data
            cov_names <-  num_covariates(X)$combined[-1] # extract names of covariates
            cols_covariates <- dplyr::select(X, cov_names, year.cts, prevalence.cts) # columns containing only covariate data
            no_cov <- num_covariates(X)$no_covariates # number of covariates
            
            
            size_study_specific <- input$size_study_specific
            size_summary <- input$size_summary

            cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
            
       if (cts_cov_indicator == 0)  { # if categorical/discrete covariate ----------------------------------------------------------------------  
         
                # count number of levels of the covariate 
                num_levels <- length(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
                
                # add study weights to dataset
                X <- MA_weights(X, mod)
                
                ## credible regions
                cred_list <- list()
                for (i in 1:num_levels) {
                  cred_list[[i]] <- tibble(y = (rstan::extract(mod, pars = "lSe")$lSe[,i]) , 
                                           x = (rstan::extract(mod, pars = "lSp")$lSp[,i]))
                }
                cred <- rbindlist(cred_list, idcol = TRUE) %>% 
                  dplyr::mutate( !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(.id))
                
                # in inv_logit space
                g <- ggplot(data = cred, 
                            aes(x = x, y = y,
                                color = !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) ))  + 
                     stat_ellipse()  
                
                g
                # Get ellipse coordinates from plot
                pb <-  ggplot_build(g)
                el = pb$data[[1]][c("x","y", "group")]
                credible_region <- tibble(x = plogis(el$x), 
                                          y = plogis(el$y), 
                                          !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(el$group))
                
                g <- ggplot(data = credible_region, 
                            aes(x = x, y = y, 
                                color = !!as.name(str_sub(colnames(X)[j + cov_index], end = -5))))  + 
                  geom_polygon(data = credible_region, aes(x = 1  - x, y = y), alpha=0.05, size=0.4)  + 
                  xlim(0,1) + 
                  ylim(0,1)
                
                ## prediction regions
                pred_list <- list()
                for (i in 1:num_levels) {
                  pred_list[[i]] <- tibble(y = (rstan::extract(mod, pars = "lSe_pred")$lSe_pred[,i]),
                                           x = (rstan::extract(mod, pars = "lSp_pred")$lSp_pred[,i]))
                }
                
                pred <- rbindlist(pred_list, idcol = TRUE) %>% 
                  dplyr::mutate(!!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(.id))
                
                # in inv_logit space
                g <- ggplot(data = pred,
                            aes(x = x, y = y, 
                                color = !!as.name(str_sub(colnames(X)[j + cov_index], end = -5))))  +
                  stat_ellipse()
                
                # Get ellipse coordinates from plot
                pb <-  ggplot_build(g)
                el = pb$data[[1]][c("x","y", "group")]
                
                pred_region <- tibble(x = plogis(el$x),
                                      y = plogis(el$y),
                                      !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(el$group))
                
                g <- ggplot(data = pred_region, aes(x = x, 
                                                    y = y,
                                                    color =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5))))  +
                  geom_polygon(data = pred_region, aes(x = 1  - x,
                                                       y = y,
                                                       color =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5))), 
                               alpha=0.05, 
                               size=0.4)  +
                  xlim(0,1) +
                  ylim(0,1)
                
                
                ## medians
                median_sens <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
                l_sens <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,4])
                u_sens <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,6])
                median_spec <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary[,5])
                l_spec <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary[,4])
                u_spec <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary[,6])
                
                medians <- tibble(median_sens = median_sens, l_sens = l_sens, u_sens = u_sens, 
                                  median_spec = median_spec, l_spec = l_spec, u_spec = u_spec, 
                                  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(seq(from = 1, to = num_levels, by = 1)))
                
                medians2 <- dplyr::mutate(medians,  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) :=
                                            factor(levels(factor(X[, j + cov_index])) ))
                
                pred_region2 <- pred_region
                for (i in 1:num_levels) {
                  pred_region2[, 3] = case_when(pred_region[, 3] == i ~ levels(factor(X[, j + cov_index]))[i] ,
                                                TRUE ~ as.character(c(pred_region2[, 3])[[1]])  )
                }
                
                credible_region2 <- credible_region
                for (i in 1:num_levels) {
                  credible_region2[, 3] = case_when(credible_region[, 3] == i ~ levels(factor(X[, j + cov_index]))[i] , 
                                                    TRUE ~ as.character(c(credible_region2[, 3])[[1]])  )
                }
                
                # observed values
                ss<- tibble( 
                  Study =as.numeric(as.factor(X$author)), 
                  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(X[, j + cov_index]), 
                  TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
                  N=(X$TP+X$FN+X$FP+X$TN) ,
                  Sensitivity= (TP/(TP+FN))  , 
                  Specificity= (TN/(TN+FP))  , 
                  prev = round((TP+FN)/N, 2), 
                  pctse = X$pctse, 
                  pctsp = X$pctsp
                )
               
                
                TPR <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("roc_points_tpr"))$summary[,5])
                FPR <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("roc_points_fpr"))$summary[,5])
                
                cov <- c()
                for (i in 1:num_levels) {
                  cov[(1+101*(i-1)):(101+101*(i-1))] <- levels(factor(X[, j + cov_index]))[i]
                }
                
                roc_points <- tibble(TPR, FPR,
                                     !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := cov)
                
                # Calculate min and max values of sens and spec to avoid extrapolation of SROC curve
                # need to calculate min and max TPR and FPR for each group
                minTPR <- c()
                maxTPR <- c()
                minFPR <- c()
                maxFPR <- c()
                roc_points2 <- list()
                
                for (i in 1:num_levels) { 
                  X_sub <- dplyr::filter(X, !!as.name(colnames(X)[j + cov_index]) == as.numeric(levels(factor(X[, j + cov_index]))[i]))
                  minTPR[i] <- min(  X_sub$TP/(X_sub$TP+X_sub$FN) )
                  maxTPR[i] <- max(  X_sub$TP/(X_sub$TP+X_sub$FN) )
                  minFPR[i] <- min(  X_sub$FP/(X_sub$FP+X_sub$TN) )
                  maxFPR[i] <- max(  X_sub$FP/(X_sub$FP+X_sub$TN) )
                  # Create new data frame which restricts roc_points to being between min and max values
                  roc_points2[[i]] <- subset(dplyr::filter(roc_points, 
                                                           !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) == 
                                                             as.numeric(levels(factor(X[, j + cov_index]))[i])), 
                                             FPR<maxFPR[i] & FPR>minFPR[i] & TPR<maxTPR[i] & TPR>minTPR[i])
                }
                roc_points_restricted <- rbindlist(roc_points2)
                roc_points_unrestricted <- roc_points
                  
                # Calculate sens and spec confidence intervals at the study level
                # Add the confidence intervals to the dataset
                foreach (i = 1:N) %do% {
                  ss$Sens_LCI[i] <- binconf(ss$TP[i], ss$TP[i]+ss$FN[i], method="exact")[2]
                  ss$Sens_UCI[i] <- binconf(ss$TP[i], ss$TP[i]+ss$FN[i], method="exact")[3]
                  ss$FPR_LCI[i]  <- 1 - binconf(ss$TN[i], ss$FP[i]+ss$TN[i], method="exact")[3]
                  ss$FPR_UCI[i]  <- 1 - binconf(ss$TN[i], ss$FP[i]+ss$TN[i], method="exact")[2]
                }
                
                ss <- cbind(ss, cols_covariates) # add covariates to data
              
                
                # Base plot (default)
                g <- ggplot(data = medians2, aes(y=median_sens, 
                                                 x = 1-median_spec,
                                                 colour =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)))) + 
                  geom_point(size = size_summary)  +      # summary points
                  geom_path(data = pred_region2, aes(x= 1 - x, y= y, 
                                                     colour =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5))), 
                            linetype = 2, size = 0.4, inherit.aes = F) +                         # prediction region
                  geom_polygon(data = credible_region2, aes(x= 1 - x, y= y,
                                                            colour =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5))),
                               alpha=0.05, size=0.4, linetype = 2,inherit.aes = F) + # conf region
                  theme_bw() + 
                  theme(text = element_text(size=15)) + 
                  ylab("Sensitivity") + 
                  xlab("1 - Specificity") + 
                  coord_fixed()
                g
                
                # make covariates factors for display
                for (i in 1:no_cov) { 
                  ss <- dplyr::mutate(ss, !!as.name(colnames(ss)[16+i]) := factor( !!as.name(colnames(ss)[16+i])))
                }
                
                
                # Plot study level estimates 
                if ('1' %in% input$HSROCcheck) {
                      if (input$weightcheck == TRUE) {
                        g = g + geom_ellipse(data = ss, 
                                             inherit.aes = F, 
                                             aes(x0 = 1-Specificity, 
                                                 y0 = Sensitivity,
                                                 a=pctsp/input$weight_scale, 
                                                 b=pctse/input$weight_scale, 
                                                 angle = 0,  
                                                 colour =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5))))
                        g
                      }
                      else {
                        g = g + geom_point(data = ss, aes(y=Sensitivity, 
                                                          x = 1-Specificity, 
                                                          colour =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5))), 
                                           size = size_study_specific,
                                           alpha=0.7,
                                           inherit.aes = F)  
                        g
                      }
                  }
                # HSROC curve 
                if ('2' %in% input$HSROCcheck) { 
                  g = g + geom_path(data = roc_points_restricted, 
                                    inherit.aes = FALSE, 
                                    aes(x = FPR, y = TPR,
                                        colour =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)))) 
                  g 
                  
                  if (input$HSROCcheck_unrestricted_HSROC_curve == TRUE) { 
                    g = g + geom_path(data = roc_points_unrestricted, 
                                      inherit.aes = FALSE, 
                                      aes(x = FPR, y = TPR,
                                          colour =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)))) 
                    g 
                  }
                }
                else { g }
                
                if ('3' %in% input$HSROCcheck) { 
                  g = g + scale_x_continuous(breaks = seq(0,1,0.2), limits = c(0,1))  + 
                          scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))  
                  g
                } else { g }
                
                # display disease prevalence's on sROC plot 
                if ( input$prevcheck== TRUE ) {  
                  g = g +   geom_label_repel(data = ss,  
                                             inherit.aes = FALSE,
                                             aes(y=Sensitivity, x = 1-Specificity, label = prev),  
                                             size = 3.5, 
                                             box.padding = 0.25)  
                  g
                } 
                else { g }
                
                
                # Plot observed sens CI's
                if ('1' %in% input$cicheck) {
                  g = g + geom_errorbar(data = ss, 
                                        # inherit.aes = F, 
                                        aes(y=Sensitivity,
                                            x = 1-Specificity,
                                            ymin = Sens_LCI,
                                            ymax = Sens_UCI,
                                            colour =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5))),
                                        width = 0.01,
                                        alpha = 0.4)  
                  g
                }
                else { g }
                # Plot observed spec CI's
                if ('2' %in% input$cicheck) {
                  g = g + geom_errorbar(data = ss, 
                                        # inherit.aes = F, 
                                        aes(y=Sensitivity, 
                                            x = 1-Specificity, 
                                            xmin = FPR_LCI,
                                            xmax = FPR_UCI,
                                            colour =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5))), 
                                        width = 0.01, 
                                        alpha = 0.4)  
                  g
                }
                else { g }
                
                # plot other covariate selected as text label 
                for (i in 1:(no_cov+2)) {
                  if (input$covcheck_display == i+1) { # the covariate selected by the user
                    if (str_sub(colnames(X)[j + cov_index], start = -3) != "cts") { # if selected display covariate is discrete / categorical
                      if (input$weightcheck == FALSE) { # without study weights
                        # plot covariates as text labels only
                        g = g + geom_label_repel(data = ss,
                                                 inherit.aes = F, 
                                                 aes(y=Sensitivity, x = 1-Specificity,
                                                     label = !!as.name(colnames(ss)[16+i])) )
                        g
                      }
                      else { g }
                      if (input$weightcheck == TRUE) { # with study weights
                        # plot covariates as text only
                        g = g + geom_label_repel(data = ss,
                                                 inherit.aes = F, 
                                                 aes(y=Sensitivity, x = 1-Specificity, 
                                                     label = !!as.name(colnames(ss)[16+i])))  +
                          geom_ellipse(data = ss,
                                       inherit.aes = F,
                                       aes(x0 = 1-Specificity, y0 = Sensitivity,
                                           colour = !!as.name(colnames(ss)[16+cov_index]),
                                           a=pctsp/input$weight_scale,
                                           b=pctse/input$weight_scale, 
                                           angle = 0))
                        
                      }
                      else { g }
                    } else { # if selected display covariate is cts. 
                      if (input$weightcheck == FALSE) { # without study weights
                        # plot covariates as text labels only
                        g = g + geom_label_repel(data = ss,
                                                 inherit.aes = F, 
                                                 aes(y=Sensitivity, 
                                                     x = 1-Specificity,
                                                     label = as.factor(!!as.name(colnames(ss)[16+i]))) ) # make cts cov. discrete for the labels
                        g
                      }
                      else { g }
                      if (input$weightcheck == TRUE) { # with study weights
                        # plot covariates as text only
                        g = g + geom_label_repel(data = ss,
                                                 inherit.aes = F, 
                                                 aes(y=Sensitivity, 
                                                     x = 1-Specificity, 
                                                     label = as.factor(!!as.name(colnames(ss)[16+i]))))  +
                          geom_ellipse(data = ss,
                                       inherit.aes = F,
                                       aes(x0 = 1-Specificity, y0 = Sensitivity,
                                           colour = (!!as.name(colnames(ss)[16+cov_index])),
                                           a=pctsp/input$weight_scale,
                                           b=pctse/input$weight_scale, 
                                           angle = 0))
                        
                      }
                      else { g }
                    }
                  }
                } # end of covariate label plotting
                
                g
              }
              
              
          #  Meta-regression - sROC plot - cts covariate ----------------------------------------------------------------
              
     else {    
 
       
                X <- MA_weights(X, mod) # extract study weights
                
                ## credible region
                cred <- tibble(y = (rstan::extract(mod, pars = "lSe_at_cov_input")$lSe_at_cov_input) ,
                               x = (rstan::extract(mod, pars = "lSp_at_cov_input")$lSp_at_cov_input))
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
                
                ## prediction region
                pred <- tibble(y = (rstan::extract(mod, pars = "lSe_pred_at_cov_input")$lSe_pred_at_cov_input), 
                               x = (rstan::extract(mod, pars = "lSp_pred_at_cov_input")$lSp_pred_at_cov_input))
                # in inv_probit space
                g <- ggplot(data = pred, aes(x = x, y = y))  + 
                  stat_ellipse()  
                # Get ellipse coordinates from plot
                pb <-  ggplot_build(g)
                el = pb$data[[1]][c("x","y", "group")]
                pred_region <- tibble(x = plogis(el$x), y = plogis(el$y))
                ## medians
                median_sens <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se_at_cov_input"))$summary[,5])
                median_spec <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp_at_cov_input"))$summary[,5])
                medians <- tibble(median_sens = median_sens, 
                                  median_spec = median_spec)
                
                # observed values
                ss<- tibble( 
                  Study =as.numeric(as.factor(X$author)), 
                  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(X[, j + cov_index]), 
                  TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
                  N=(X$TP+X$FN+X$FP+X$TN) ,
                  Sensitivity= (TP/(TP+FN))  , 
                  Specificity= (TN/(TN+FP))  , 
                  prev = round((TP+FN)/N, 2), 
                  pctse = X$pctse, 
                  pctsp = X$pctsp
                )
              
                # Add the HSROC curve
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
                                                FPR<maxFPR & FPR>minFPR & TPR<maxTPR & TPR>minTPR)
                roc_points_unrestricted <- roc_points
                
                # Calculate sens and spec confidence intervals at the study level
                # Add the confidence intervals to the dataset
                foreach (i = 1:N) %do% {
                  ss$Sens_LCI[i] <- binconf(ss$TP[i], ss$TP[i]+ss$FN[i], method="exact")[2]
                  ss$Sens_UCI[i] <- binconf(ss$TP[i], ss$TP[i]+ss$FN[i], method="exact")[3]
                  ss$FPR_LCI[i]  <- 1 - binconf(ss$TN[i], ss$FP[i]+ss$TN[i], method="exact")[3]
                  ss$FPR_UCI[i]  <- 1 - binconf(ss$TN[i], ss$FP[i]+ss$TN[i], method="exact")[2]
                }
                
                ss <- cbind(ss, cols_covariates) # add covariates to data
                
                ss_unfct <- ss
                
                # make covariates factors for display
                for (i in 1:no_cov) { 
                
                  ss_unfct <- dplyr::mutate(ss_unfct,
                                            !!as.name(paste0(colnames(ss)[16+i],", categorical")) :=  !!as.name(colnames(ss)[16+i]))
                  
                  ss <- dplyr::mutate(ss,
                                      !!as.name(colnames(ss)[16+i]) := factor( !!as.name(colnames(ss)[16+i])))

                }
                
                print(colnames(ss)[16+cov_index])
                
                ### Base Plot 
                # Base plot (default)
                g <- ggplot(data = medians, aes(y=median_sens, 
                                                x = 1-median_spec)) + 
                  geom_point(size = size_summary)  +      # summary points
                  geom_path(data = pred_region, aes(x= 1 - x, y= y), linetype = 2, size = 0.4, inherit.aes = F) +                         # prediction region
                  geom_polygon(data = credible_region, aes(x= 1 - x, y= y), alpha=0.05, size=0.4, linetype = 2,inherit.aes = F) + # conf region
                  theme_bw() + 
                  theme(text = element_text(size=15)) + 
                  ylab("Sensitivity") + 
                  xlab("1 - Specificity") + 
                  coord_fixed()
                g
                
                # Plot study level estimates 
                if ('1' %in% input$HSROCcheck) {
                  if (input$cts_as_fct == TRUE) { # plot cts covariate as categorical
                            if (input$weightcheck == TRUE) {
                              g = g + geom_ellipse(data = ss, 
                                                   aes(x0 = 1-Specificity, 
                                                       y0 = Sensitivity, 
                                                       a=pctsp/input$weight_scale,
                                                       b=pctse/input$weight_scale, 
                                                       colour = !!as.name(colnames(ss)[16+cov_index]),
                                                       angle = 0)) 
                              g
                            }
                            else {
                              g = g + geom_point(data = ss, 
                                                 aes(y = Sensitivity,
                                                     x = 1-Specificity, 
                                                     colour = !!as.name(colnames(ss)[16+cov_index])), 
                                                 size = size_study_specific, 
                                                 alpha=0.7,
                                                 inherit.aes = F) 
                              g
                            }
                  } else { # plot cts covariate as cts
                    # ss <- dplyr::mutate(ss, 
                    #                     !!as.name(paste0(colnames(ss)[16+cov_index],", categorical")) := 
                    #                       varhandle::unfactor( !!as.name(colnames(ss)[16+cov_index]) ) )
                    
                    if (input$weightcheck == TRUE) {
                      g = g + geom_ellipse(data = ss_unfct, 
                                           aes(x0 = 1-Specificity, 
                                               y0 = Sensitivity, 
                                               a=pctsp/input$weight_scale,
                                               b=pctse/input$weight_scale, 
                                               colour = !!as.name(colnames(ss)[16+cov_index]), 
                                               angle = 0))
                      g
                    }
                    else {
                      g = g + geom_point(data = ss_unfct, 
                                         aes(y = Sensitivity,
                                             x = 1-Specificity, 
                                         colour = !!as.name(colnames(ss)[16+cov_index])), 
                                         size = size_study_specific, 
                                         alpha=0.7)
                      g
                    }
                  }
                }
                
                
                # HSROC curve 
                if ('2' %in% input$HSROCcheck) {
                  
                  g = g + geom_path(data = roc_points_restricted, 
                                    aes(x = FPR, y = TPR)) 
                  g 
                  
                  if (input$HSROCcheck_unrestricted_HSROC_curve == TRUE) { 
                    g = g + geom_path(data = roc_points_unrestricted, 
                                      aes(x = FPR, y = TPR)) 
                    g 
                  }
                }
                else { g }
                
                
                if ('3' %in% input$HSROCcheck) { 
                  g = g + scale_x_continuous(breaks = seq(0,1,0.2), limits = c(0,1))  + 
                          scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))  
                  g
                } else { g }
                
                # display disease prevalence's on sROC plot 
                if ( input$prevcheck == TRUE ) {  
                  g = g +   geom_label_repel(data = ss ,
                                             inherit.aes = F,
                                             aes(y = Sensitivity, 
                                                 x = 1-Specificity,
                                                 label = prev), 
                                             size = 3.5,
                                             box.padding = 0.25)  
                  g
                }
                else { g }
                
                
                # Plot observed sens CI's
                if ('1' %in% input$cicheck) {
                  g = g + geom_errorbar(data = ss, 
                                        inherit.aes = F, 
                                        aes(y=Sensitivity, 
                                            x = 1-Specificity, 
                                            ymin = Sens_LCI, 
                                            ymax = Sens_UCI), 
                                        width = 0.01, 
                                        alpha = 0.4)  
                  g
                }
                else { g }
                
                # Plot observed spec CI's
                if ('2' %in% input$cicheck) {
                  g = g + geom_errorbar(data = ss, 
                                        aes(x = 1-Specificity, 
                                            y = Sensitivity, 
                                            xmin = FPR_LCI,
                                            xmax = FPR_UCI),
                                        width = 0.01,
                                        alpha = 0.4,
                                        inherit.aes = F)  
                  g
                }
                else { g }
                
                # If using Cov.csv
                if (C > 6 & Names[7] != "rob_PS") {  
                  # Plot observed sens CI's
                  if ('1' %in% input$cicheck) {
                    g = g + geom_errorbar(data = ss, aes(x = 1-Specificity, 
                                                         y = Sensitivity, 
                                                         ymin = Sens_LCI,
                                                         ymax = Sens_UCI),
                                          width = 0.02, 
                                          alpha = 1,
                                          inherit.aes = F) 
                    g
                  }
                  else { g }
                  # Plot observed spec CI's
                  if ('2' %in% input$cicheck) {
                    g = g + geom_errorbar(data = ss, aes(x = 1-Specificity, 
                                                         y = Sensitivity, 
                                                         xmin = FPR_LCI,
                                                         xmax = FPR_UCI),
                                          width = 0.02, 
                                          alpha = 1,
                                          inherit.aes = F)  
                    g
                  }
                  else { g }
                }
                
                # plot other covariate selected as text label
                for (i in 1:(no_cov+2)) {
                  if (input$covcheck_display == i+1) { # the covariate selected by the user
                    if (str_sub(colnames(X)[j + cov_index], start = -3) != "cts") { # if selected display covariate is discrete / categorical
                      if (input$weightcheck == FALSE) { # without study weights
                        # plot covariates as text labels only
                        g = g + geom_label_repel(data = ss,
                                                 inherit.aes = F, 
                                                 aes(y=Sensitivity, x = 1-Specificity,
                                                     label = !!as.name(colnames(ss)[16+i])) )
                        g
                      }
                      else { g }
                      if (input$weightcheck == TRUE) { # with study weights
                        # plot covariates as text only
                        g = g + geom_label_repel(data = ss,
                                                 inherit.aes = F, 
                                                 aes(y=Sensitivity, 
                                                     x = 1-Specificity, 
                                                     label = !!as.name(colnames(ss)[16+i])))  +
                          geom_ellipse(data = ss,
                                       inherit.aes = F,
                                       aes(x0 = 1-Specificity, 
                                           y0 = Sensitivity,
                                           colour = !!as.name(colnames(ss)[16+cov_index]),
                                           a=pctsp/input$weight_scale,
                                           b=pctse/input$weight_scale, 
                                           angle = 0))
                        
                      }
                      else { g }
                    } else { # if selected display covariate is cts. 
                      if (input$weightcheck == FALSE) { # without study weights
                        # plot covariates as text labels only
                        g = g + geom_label_repel(data = ss,
                                                 inherit.aes = F, 
                                                 aes(y=Sensitivity, 
                                                     x = 1-Specificity,
                                                     label = as.factor(!!as.name(colnames(ss)[16+i]))) ) # make cts cov. discrete for the labels
                        g
                      }
                      else { g }
                      if (input$weightcheck == TRUE) { # with study weights
                        # plot covariates as text only
                        g = g + geom_label_repel(data = ss,
                                                 inherit.aes = F, 
                                                 aes(y=Sensitivity, 
                                                     x = 1-Specificity, 
                                                     label = as.factor(!!as.name(colnames(ss)[16+i]))))  +
                          geom_ellipse(data = ss,
                                       inherit.aes = F,
                                       aes(x0 = 1-Specificity, y0 = Sensitivity,
                                           colour = (!!as.name(colnames(ss)[16+cov_index])),
                                           a=pctsp/input$weight_scale,
                                           b=pctse/input$weight_scale, 
                                           angle = 0))
                        
                      }
                      else { g }
                    }
                  }
                } # end of covariate label plotting
                g
              }

            g
    })
      

      # Output ggplot object
    observe({
       output$plot <- renderPlot({  
              
                      validate(
                        need(!(is.null(draws())), 
                             "Please run model to display plot")
                      )
                      
                      
                      req(draws(), cancelOutput = TRUE)
                      
                      plot_object()
                      
         }, height = as.numeric(input$plot_dimension_slider))
    })
    
      # Download ggplot object 
      output$plot_download <- downloadHandler(
            filename = function(){
              paste("plot.png")
            },
            content = function(file) { 
              ggsave(file,
                     plot_object(),
                     width = input$plot_width,
                     height = input$plot_height,
                     dpi = input$plot_dpi,
                     units = "in")
            }
      )
      
     # tooltips  -----------------------------------------------------------------------------------------------
      
      click <- reactive({  input$plot_click })
      
      # values to display on plot
      output$vals <- renderPrint({
                            
                            validate(
                              need(!(is.null(draws())), 
                                   "Please run model to display plot")
                            )
                            
                            
                            req(draws(), cancelOutput = TRUE)
                            
                            X <- data()
                            
                            cov_index <-  as.integer(as.double(input$covcheck_model)) - 1
                            N <- nrow(X)
                            
                            C <- ncol(X)
                            Names <- colnames(X)
                            
                            if (C > 8 & Names[7] != "rob_PS") { 
                              j <<- 6 
                              no_covariates <<- length(colnames(X[,7:C])) 
                            } else { 
                              j <<- 13 
                              no_covariates <<- length(colnames(X[,14:C])) 
                            }
                            
                            # observed values
                            ss<- tibble( 
                              Study =as.numeric(as.factor(X$author)), 
                              TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
                              N=(X$TP+X$FN+X$FP+X$TN) ,
                              Sensitivity= (TP/(TP+FN))  , 
                              Specificity= (TN/(TN+FP))  , 
                              FPR = 1 - Specificity,
                              prev = round((TP+FN)/N, 2), 
                              !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := (X[, j + cov_index])
                            )
                    
                            ss2 <- left_join(ss, X)
                            
                            
                            # make cols without .cts and .cat endings for display
                            for (i in 1:no_covariates) { 
                              ss2 <- dplyr::mutate(ss2,
                                                   !!as.name( str_sub(colnames(X)[j + i], end = -5) ) := !!as.name( colnames(X)[j + i]) )
                            }
                            
                            # select columns to display
                            cols <- c("author", "year", 
                                      str_sub( colnames(X)[c( (j+1):(j - 1 + no_covariates) )] , end = -5), 
                                      "Sensitivity", "Specificity", "FPR")
                            data <- dplyr::select(ss2, cols)
                            data[] <- lapply(data, function(x) if (is.numeric(x) ) round(x, 3) else x) # round all numeric cols to 3 s.f. 
                            
                            data <- data.frame(data)
                            
                            y <- nearPoints(df = data, click() , xvar = "FPR", yvar = "Sensitivity" )
                            req(nrow(y) != 0)
                            
                            y
        
      })
      
      # tooltip
      output$my_tooltip <- renderUI({
                            
                            validate(
                              need(!(is.null(draws())), 
                                   "Please run model to display plot")
                            )
                            
                            
                            req(draws(), cancelOutput = TRUE)
                            
                            ns <- session$ns
                            
                            X <- data()
                            
                            cov_index <-  as.integer(as.double(input$covcheck_model)) - 1
                            N <- nrow(X)
                            
                            C <- ncol(X)
                            Names <- colnames(X)
                            
                            if (C > 8 & Names[7] != "rob_PS") { 
                              j <<- 6 
                              no_covariates <<- length(colnames(X[,7:C])) 
                            } else { 
                              j <<- 13 
                              no_covariates <<- length(colnames(X[,14:C])) 
                              }
                            
                            # observed values
                            ss<- tibble( 
                              Study =as.numeric(as.factor(X$author)), 
                              TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
                              N=(X$TP+X$FN+X$FP+X$TN) ,
                              Sensitivity= (TP/(TP+FN))  , 
                              Specificity= (TN/(TN+FP))  , 
                              FPR = 1 - Specificity,
                              prev = round((TP+FN)/N, 2), 
                              !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := (X[, j + cov_index])
                            )
                            
                            ss2 <- left_join(ss, X)
                            
                            
                            # make cols without .cts and .cat endings for display
                            for (i in 1:no_covariates) { 
                              ss2 <- dplyr::mutate(ss2,
                                                   !!as.name( str_sub(colnames(X)[j + i], end = -5) ) := !!as.name( colnames(X)[j + i]) )
                            }
                            
                            # select columns to display
                            cols <- c("author", "year", 
                                      str_sub( colnames(X)[c( (j+1):(j - 1 + no_covariates) )] , end = -5), 
                                      "Sensitivity", "Specificity", "FPR")
                            data <- dplyr::select(ss2, cols)
                            data[] <- lapply(data, function(x) if (is.numeric(x) ) round(x, 3) else x) # round all numeric cols to 3 s.f. 
                            data <- data.frame(data)
                            
                            y <- nearPoints(df = data, click(), xvar = "FPR", yvar = "Sensitivity" )
                            req(nrow(y) != 0)
                            
                            verbatimTextOutput(ns("vals"))
        
      })
      
      
    }
  )
}


















