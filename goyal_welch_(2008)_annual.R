#  -----------------------------------------------------------------------------
#
# Title : Goyal & Welch (2008) Replication
#    By : Daniel A. Amy
#  Date : 2020-03-16
#
#  -----------------------------------------------------------------------------

#   ____________________________________________________________________________
#   ATTACH PACKAGES                                                         ####
#   ____________________________________________________________________________

    library(data.table)
    library(tidyverse)
    library(lubridate)
    library(magrittr)
    library(dyn)
    library(reshape2)
    library(formattable)
    library(sparkline)
    library(xtable)
    library(DT)
    library(htmltools)
    library(dplyr)
    library(tidyr)

#   ____________________________________________________________________________
#   IMPORT DATA                                                             ####
#   ____________________________________________________________________________

    annual <- fread(file.path("/Users/danielamy/Google Drive/[Ph.D.]/[Courses]/Year 1/Summer/[FIN 7806]/Goyal and Welch (2008)/Data/annual_2018.csv"))
    
    table1_2005_g_w <- fread(file.path("/Users/danielamy/Google Drive/[Ph.D.]/[Courses]/Year 1/Summer/[FIN 7806]/Goyal and Welch (2008)/Data/annual_2005_goyal_welch.csv"))

#   ____________________________________________________________________________
#   DEFINE VARIABLES                                                        ####
#   ____________________________________________________________________________

##  ............................................................................
##  Default yield spread (dfy)                                              ####
##  ............................................................................
##  difference between BAA and AAA-rated corporate bond yields              

    annual <- annual[, dfy := BAA - AAA]

##  ............................................................................
##  Default Return Spread (dfr)                                             ####
##  ............................................................................
##  difference between long-term corporate bond and long-term government bonds

    annual <- annual[, dfr := corpr - ltr] 

##  ............................................................................
##  Stock returns (IndexDiv)                                                ####
##  ............................................................................
##  continuously compounded S&P 500 returns (Index) + 12-month moving sum of
##  S&P Dividends (D12)
    
    annual$Index <- as.numeric(annual$Index) # In 2018 dataset Index is set to 'chr' datatype 
    annual <- annual[, IndexDiv := Index + D12]

##  ............................................................................
##  Dividend-price ratio (dp)                                               ####
##  ............................................................................
##  difference between the log of dividends and the log of prices           

    annual <- annual[, dp := log(D12) - log(Index)]

##  ............................................................................
##  Log 3-month treasury bill (logRfree)                                    ####
##  ............................................................................

    annual <- annual[, logRfree := log(Rfree + 1)]

##  ............................................................................
##  Term spread (tms)                                                       ####
##  ............................................................................
##  difference between the long term yield on government bonds (lty) and the
##  treasury-bill (Rfree)                                                   

    annual <- annual[, tms := lty - tbl]

##  ............................................................................
##  Dividend yield vector (vec_dy)                                          ####
##  ............................................................................
##  difference between the log of dividends and the log of lagged prices [P(t-1)]

    vec_dy <- c(NA, annual[2:nrow(annual), log(D12)] - annual[1:(nrow(annual)-1), log(Index)])

##  ............................................................................
##  Dividend vector (dy)                                                    ####
##  ............................................................................

    annual <- annual[, dy := vec_dy]

    annual <- annual[, logret   := c(NA,diff(log(Index)))]

    vec_logretdiv <- c(NA, annual[2:nrow(annual), log(IndexDiv)] - annual[1:(nrow(annual)-1), log(Index)])

    vec_logretdiv <- c(NA, log(annual[2:nrow(annual), IndexDiv]/annual[1:(nrow(annual)-1), Index]))

    annual <- annual[, logretdiv := vec_logretdiv]

##  ............................................................................
##  Earnings price ratio (ep)                                               ####
##  ............................................................................
##  difference between the log of earnings (E12) and the log of prices (Index)

    annual <- annual[, ep := log(E12) - log(Index)]

##  ............................................................................
##  Dividend payout ratio (de)                                              ####
##  ............................................................................
##  difference between the log of dividends (D12) and the log of earnings (E12)

    annual <- annual[, de := log(D12) - log(E12)] 

##  ............................................................................
##  Log equity premium (rp_div)                                             ####
##  ............................................................................

    annual <- annual[, rp_div := logretdiv - logRfree]

#   ____________________________________________________________________________
#   TIME SERIES                                                             ####
#   ____________________________________________________________________________

##  ............................................................................
##  Annual data time series object (ts_annual)                              ####
##  ............................................................................

    ts_annual <- ts(annual, start=annual[1, yyyy], end=annual[nrow(annual), yyyy])

##  ............................................................................
##  Plot ts_annual                                                          ####
##  ............................................................................
##  log equity premium, dividend-price ratio, dividend yield

    plot(ts_annual[, c("rp_div", "dp", "dy")])

#   ____________________________________________________________________________
#   STATISTICS FUNCTION                                                     ####
#   ____________________________________________________________________________
#   time series data frame (ts_df), independent variable (indep), dependent
#   variable (dep) , degree of overlap (h), starting date (start), ending date
#   (end), number of OOS periods used (est_periods_OOS)

    get_statistics <- function(ts_df, indep, dep, h=1, start=1872, end=2005, est_periods_OOS = 20)
  
    {
  
  ##  ............................................................................
  ##  In Sample                                                               ####
  ##  ............................................................................
  
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### IS Historical mean model                                                ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### na.rm=TRUE - Excludes missing values from analyses                      
  
      avg <- mean(window(ts_df, start, end)[, dep], na.rm=TRUE)
  
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### IS historical mean error                                                ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  
      IS_error_N <- (window(ts_df, start, end)[, dep] - avg)
  
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### IS OLS Regression - dyn package used to regress using lagged variable   ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  
      reg <- dyn$lm(eval(parse(text=dep)) ~ stats::lag(eval(parse(text=indep)), -1), data=window(ts_df, start, end))
  
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### IS OLS error                                                            ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  
      IS_error_A <- reg$residuals
  
  ##  ............................................................................
  ##  Out Of Sample                                                           ####
  ##  ............................................................................
  ##  Uses recursive estimation window                                        ####
  
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Create OOS historical mean error as "numeric" data type                 ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  
      OOS_error_N <- numeric(end - start - est_periods_OOS)
  
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Create OOS OLS error as "numeric" data type                             ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  
  
      OOS_error_A <- numeric(end - start - est_periods_OOS)
  
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Only use information that is available up to the time of forecast       ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  
      j <- 0
  
      for (i in (start + est_periods_OOS):(end-1)) 
    
      {
    
      j <- j + 1
    
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Actual ERP                                                              ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
    
      actual_ERP <- as.numeric(window(ts_df, i+1, i+1)[, dep])
    
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### OOS Historical mean model                                               ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
    
      OOS_error_N[j] <- actual_ERP - mean(window(ts_df, start, i)[, dep], na.rm=TRUE)
    
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### OOS OLS model                                                           ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
    
      reg_OOS <- dyn$lm(eval(parse(text=dep)) ~ stats::lag(eval(parse(text=indep)), -1), data=window(ts_df, start, i))
    
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### OOS OLS error                                                           ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
    
      df <- data.frame(x=as.numeric(window(ts_df, i, i)[, indep]))
      names(df) <- indep
      pred_ERP   <- predict.lm(reg_OOS, newdata=df)
      OOS_error_A[j] <-  pred_ERP - actual_ERP
    
      }
  
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Historical mean model                                                   ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### vector of rolling OOS errors                                            
  
      MSE_N <- mean(OOS_error_N^2)
  
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### OLS model                                                               ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### vector of rolling OOS errors                                            
  
      MSE_A <- mean(OOS_error_A^2) 
  
      T <- length(!is.na(ts_df[, dep]))
  
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### OOS R-squared                                                           ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  
      OOS_R2  <- 1 - MSE_A/MSE_N
  
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### OOS Adjusted R-squared (OOS_aR2)                                        ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  
  # 1. df is off by 20 (amount of rolling window)
  # 2. T is not calculated correctly - 135 for all variables
      
      # OOS_aR2 <- 1 - (((1-OOS_R2)*((reg_OOS$df.residual-20)))/(reg_OOS$df.residual-21))
      OOS_aR2 <- 1 - (((1-OOS_R2)*((reg_OOS$df.residual)))/(reg_OOS$df.residual-1))
    
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Delta root mean squared error (dRMSE)                                   ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### difference between the historical mean model RMSE and the OLS model RMSE
 
      dRMSE <- sqrt(MSE_N) - sqrt(MSE_A)
  
  #   ____________________________________________________________________________
  #   CREATE PLOT                                                             ####
  #   ____________________________________________________________________________

  ##  ............................................................................
  ##  IS Cummulative SSE Difference                                           ####
  ##  ............................................................................

      IS  <- cumsum(IS_error_N[2:length(IS_error_N)]^2)-cumsum(IS_error_A^2)
  
  ##  ............................................................................
  ##  OOS Cummulative SSE Difference                                          ####
  ##  ............................................................................

      OOS <- cumsum(OOS_error_N^2)-cumsum(OOS_error_A^2)
  
      df  <- data.frame(x=seq.int(from=start + 1 + est_periods_OOS, to=end), 
                      IS=IS[(1 + est_periods_OOS):length(IS)], 
                      OOS=OOS) # One observation lost due to the lag
  
  ##  ............................................................................
  ##  Shift IS errors vertically                                              ####
  ##  ............................................................................
  ##  Sets IS line to begin at zero on the date of first OOS prediction  
  
      df$IS <- df$IS - df$IS[1] 
      df  <- melt(df, id.var="x") 
      plotGG <- ggplot(df) + 
      geom_line(aes(x=x, y=value,color=variable)
                ) + 

  ##  ............................................................................
  ##  Highlight oil shock of 1974                                             ####
  ##  ............................................................................

      geom_rect(
                data=data.frame(),# Needed by ggplot2 for transparency
                aes(xmin=1974, xmax=1975,ymin=-0.2,ymax=0.2),
                fill='red',
                alpha=0.1
      ) +
      
      # theme(plot.title = element_text(hjust = 0.5)
      #       ) + 
      #   
      # theme(plot.subtitle = element_text(vjust = 1), 
      #       plot.caption = element_text(vjust = 1), 
      #       panel.grid.major = element_line(linetype = "blank"), 
      #       panel.grid.minor = element_line(linetype = "blank"), 
      #       panel.background = element_rect(fill = "white"), 
      #       plot.background = element_rect(fill = "white")
      #       ) +
      # 
      # labs(colour = "Variable"
      #      ) + 
      #   
      # geom_vline(xintercept = 1974, size = 2, alpha = .2, color = "red"
      #            ) +

      annotate(geom = "text", x = 1973, y = 0.15, label = "Oil Shock", color = "Black",
               angle = 90, size = 3
               ) +

      annotate(geom = "text", x = 1976, y = 0.15, label = "1974", color = "Black",
               angle = 90, size = 3
               ) +
        
  ##  ............................................................................
  ##  Label x-axis and y-axis                                                 ####
  ##  ............................................................................

      scale_x_continuous('Year') +
      scale_y_continuous('Cumulative SSE Difference', limits=c(-0.2, 0.2)) 

  ##  ............................................................................
  ##  Return function values                                                  ####
  ##  ............................................................................
  
      return(list(IS_error_N = IS_error_N,
                  IS_error_A = reg$residuals,
                  OOS_error_N = OOS_error_N,
                  OOS_error_A = OOS_error_A,
                  IS_R2 = summary(reg)$r.squared, 
                  IS_aR2 = summary(reg)$adj.r.squared, 
                  OOS_R2  = OOS_R2,
                  OOS_aR2 = OOS_aR2,
                  dRMSE = dRMSE,
                  OOS_error_df = reg_OOS$df.residual,
                  OOS_T = T,
                  plotGG = plotGG
            ))
      }

#   ____________________________________________________________________________
#   STATISTICS FUNCTION (2)                                                 ####
#   ____________________________________________________________________________
#   Workaround function to adjust chart scale for stock variance (svar_stat)

    get_statistics2 <- function(ts_df, indep, dep, h=1, start=1872, end=2005, est_periods_OOS = 20)
      
    {
      
  ##  ............................................................................
  ##  In Sample                                                               ####
  ##  ............................................................................
      
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### IS Historical mean model                                                ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### na.rm=TRUE - Excludes missing values from analyses                      
      
      avg <- mean(window(ts_df, start, end)[, dep], na.rm=TRUE)
      
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### IS historical mean error                                                ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
      
      IS_error_N <- (window(ts_df, start, end)[, dep] - avg)
      
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### IS OLS Regression - dyn package used to regress using lagged variable   ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
      
      reg <- dyn$lm(eval(parse(text=dep)) ~ stats::lag(eval(parse(text=indep)), -1), data=window(ts_df, start, end))
      
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### IS OLS error                                                            ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
      
      IS_error_A <- reg$residuals
      
  ##  ............................................................................
  ##  Out Of Sample                                                           ####
  ##  ............................................................................
  ##  Uses recursive estimation window                                        ####
      
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Create OOS historical mean error as "numeric" data type                 ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
      
      OOS_error_N <- numeric(end - start - est_periods_OOS)
      
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Create OOS OLS error as "numeric" data type                             ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
      
      OOS_error_A <- numeric(end - start - est_periods_OOS)
      
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Only use information that is available up to the time of forecast       ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
      
      j <- 0
      
      for (i in (start + est_periods_OOS):(end-1)) 
        
      {
        
      j <- j + 1
        
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Actual ERP                                                              ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
        
      actual_ERP <- as.numeric(window(ts_df, i+1, i+1)[, dep])
        
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### OOS Historical mean model                                               ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
        
      OOS_error_N[j] <- actual_ERP - mean(window(ts_df, start, i)[, dep], na.rm=TRUE)
        
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### OOS OLS model                                                           ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
        
      reg_OOS <- dyn$lm(eval(parse(text=dep)) ~ stats::lag(eval(parse(text=indep)), -1), data=window(ts_df, start, i))
        
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### OOS OLS error                                                           ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
        
      df <- data.frame(x=as.numeric(window(ts_df, i, i)[, indep]))
      names(df) <- indep
      pred_ERP   <- predict.lm(reg_OOS, newdata=df)
      OOS_error_A[j] <-  pred_ERP - actual_ERP
        
      }
      
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Historical mean model                                                   ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### vector of rolling OOS errors                                            
      
      MSE_N <- mean(OOS_error_N^2)
      
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### OLS model                                                               ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### vector of rolling OOS errors                                            
      
      MSE_A <- mean(OOS_error_A^2) 
      
      T <- length(!is.na(ts_df[, dep]))
      
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### OOS R-squared                                                           ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
      
      OOS_R2  <- 1 - MSE_A/MSE_N
      
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### OOS Adjusted R-squared (OOS_aR2)                                        ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
      
      OOS_aR2 <- 1 - (((1-OOS_R2)*(reg_OOS$df.residual))/(reg_OOS$df.residual - 1))
      
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Delta root mean squared error (dRMSE)                                   ####
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### difference between the historical mean model RMSE and the OLS model RMSE
      
      dRMSE <- sqrt(MSE_N) - sqrt(MSE_A)
      
  #   ____________________________________________________________________________
  #   CREATE PLOT                                                             ####
  #   ____________________________________________________________________________
      
  ##  ............................................................................
  ##  IS Cummulative SSE Difference                                           ####
  ##  ............................................................................
      
      IS  <- cumsum(IS_error_N[2:length(IS_error_N)]^2)-cumsum(IS_error_A^2)
      
  ##  ............................................................................
  ##  OOS Cummulative SSE Difference                                          ####
  ##  ............................................................................
      
      OOS <- cumsum(OOS_error_N^2)-cumsum(OOS_error_A^2)
      
      df  <- data.frame(x=seq.int(from=start + 1 + est_periods_OOS, to=end), 
                        IS=IS[(1 + est_periods_OOS):length(IS)], 
                        OOS=OOS) # One observation lost due to the lag
      
  ##  ............................................................................
  ##  Shift IS errors vertically                                              ####
  ##  ............................................................................
  ##  Sets IS line to begin at zero on the date of first OOS prediction       

      df$IS <- df$IS - df$IS[1] 
      df  <- melt(df, id.var="x") 
      plotGG <- ggplot(df) + 
        geom_line(aes(x=x, y=value,color=variable)) + 
        
  ##  ............................................................................
  ##  Highlight oil shock of 1974                                             ####
  ##  ............................................................................
      
      geom_rect(
                data=data.frame(),# Needed by ggplot2 for transparency
                aes(xmin=1973, xmax=1975,ymin=-1.0,ymax=0.2), 
                fill='red',
                alpha=0.1
      ) + 
        
      annotate(geom = "text", x = 1972, y = 0.15, label = "Oil Shock", color = "Black",
               angle = 90, size = 3
      ) +
        
      annotate(geom = "text", x = 1976, y = 0.15, label = "1974", color = "Black",
               angle = 90, size = 3
      ) +
        
  ##  ............................................................................
  ##  Label x-axis and y-axis                                                 ####
  ##  ............................................................................
      
      scale_x_continuous('Year') +
      scale_y_continuous('Cumulative SSE Difference', limits=c(-1.0, 0.2)) 
      
  ##  ............................................................................
  ##  Return function values                                                  ####
  ##  ............................................................................
      
      return(list(IS_error_N = IS_error_N,
                  IS_error_A = reg$residuals,
                  OOS_error_N = OOS_error_N,
                  OOS_error_A = OOS_error_A,
                  IS_R2 = summary(reg)$r.squared, 
                  IS_aR2 = summary(reg)$adj.r.squared, 
                  OOS_R2  = OOS_R2,
                  OOS_aR2 = OOS_aR2,
                  dRMSE = dRMSE,
                  plotGG = plotGG
      ))
    }

#   ____________________________________________________________________________
#   CREATE PLOTS & STATISTICS                                               ####
#   ____________________________________________________________________________

##  ............................................................................
##  Dividend-price ratio (dp_stat)                                          ####
##  ............................................................................

    dp_stat <- get_statistics(ts_annual, "dp", "rp_div", start=1872)
    dp_stat$plotGG + ggtitle("Dividend-Price Ratio (dp)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Dividend-yield (dy_stat)                                                ####
##  ............................................................................

    dy_stat <- get_statistics(ts_annual, "dy", "rp_div", start=1872)
    dy_stat$plotGG + ggtitle("Dividend Yield (dy)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Earnings-price ratio (ep_stat)                                          ####
##  ............................................................................

    ep_stat <- get_statistics(ts_annual, "ep", "rp_div", start=1872)
    ep_stat$plotGG + ggtitle("Earnings-Price Ratio (ep)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Dividend payout ratio (de_stat)                                         ####
##  ............................................................................

    de_stat <- get_statistics(ts_annual, "de", "rp_div", start=1872)
    de_stat$plotGG + ggtitle("Dividend Payout Ratio (de)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Stock variance (svar_stat)                                              ####
##  ............................................................................

    svar_stat <- get_statistics2(ts_annual, "svar", "rp_div", start=1885)
    svar_stat$plotGG + ggtitle("Stock Variance (svar)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Treasury bill rate (tbl_stat)                                           ####
##  ............................................................................

    tbl_stat <- get_statistics(ts_annual, "tbl", "rp_div", start=1920)
    tbl_stat$plotGG + ggtitle("Treasury Bill Rate (tbl)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Long-term yield (lty_stat)                                              ####
##  ............................................................................

    lty_stat <- get_statistics(ts_annual, "lty", "rp_div", start=1919)
    lty_stat$plotGG + ggtitle("Long-Term Yield (lty)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Long-term return (ltr_stat)                                             ####
##  ............................................................................

    ltr_stat <- get_statistics(ts_annual, "ltr", "rp_div", start=1926)
    ltr_stat$plotGG + ggtitle("Long-Term Return (ltr)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Term spread (tms_stat)                                                  ####
##  ............................................................................

    tms_stat <- get_statistics(ts_annual, "tms", "rp_div", start=1920)
    tms_stat$plotGG + ggtitle("Term Spread (tms)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Default yield spread (dfy_stat)                                         ####
##  ............................................................................

    dfy_stat <- get_statistics(ts_annual, "dfy", "rp_div", start=1919)
    dfy_stat$plotGG + ggtitle("Default Yield Spread (dfy)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Default return spread (dfr_stat)                                        ####
##  ............................................................................

    dfr_stat <- get_statistics(ts_annual, "dfr", "rp_div", start=1926)
    dfr_stat$plotGG + ggtitle("Default Return Spread (dfr)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Inflation (infl_stat)                                                   ####
##  ............................................................................

    infl_stat <- get_statistics(ts_annual, "infl", "rp_div", start=1919)
    infl_stat$plotGG + ggtitle("Inflation (infl)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Book to market (bm_stat)                                                ####
##  ............................................................................

    bm_stat <- get_statistics(ts_annual, "bm", "rp_div", start=1921)
    bm_stat$plotGG + ggtitle("Book to Market (bm)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Investment-capital ratio (ik_stat)                                      ####
##  ............................................................................

    ik_stat <- get_statistics(ts_annual, "ik", "rp_div", start=1947)
    ik_stat$plotGG + ggtitle("Investment-Capital Ratio (ik)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Net equity expansion (ntis_stat)                                        ####
##  ............................................................................

    ntis_stat <- get_statistics(ts_annual, "ntis", "rp_div", start=1927)
    ntis_stat$plotGG + ggtitle("Net Equity Expansion (ntis)") + theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  Percent equity issuing (eqis_stat)                                      ####
##  ............................................................................

    eqis_stat <- get_statistics(ts_annual, "eqis", "rp_div", start=1927)
    eqis_stat$plotGG + ggtitle("Percent Equity Issuing (eqis)") 

#   ____________________________________________________________________________
#   TABLE 1                                                                 ####
#   ____________________________________________________________________________

##  ............................................................................
##  Create tibble                                                           ####
##  ............................................................................

    table1 <- tibble(
  
      Variable = c(
                   "dfy",
                   "infl",
                   "svar",
                   "d/e",
                   "lty",
                   "tms",
                   "tbl",
                   "dfr",
                   "d/p",
                   "d/y",
                   "ltr",
                   "e/p",
                   "b/m",
                   "i/k",
                   "ntis",
                   "eqis"
                  ),
        
        IS_aR2 = c(
                   format(round(dfy_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(infl_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(svar_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(de_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(lty_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(tms_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(tbl_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(dfr_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(dp_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(dy_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(ltr_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(ep_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(bm_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(ik_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(ntis_stat$IS_aR2*100,digits=2),nsmall=2),
                   format(round(eqis_stat$IS_aR2*100,digits=2),nsmall=2)
                  ),
        
       OOS_aR2 = c(
                   format(round(dfy_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(infl_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(svar_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(de_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(lty_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(tms_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(tbl_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(dfr_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(dp_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(dy_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(ltr_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(ep_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(bm_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(ik_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(ntis_stat$OOS_aR2*100,digits=2),nsmall=2),
                   format(round(eqis_stat$OOS_aR2*100,digits=2),nsmall=2)
                 ),
       
         dRMSE = c(
                   format(round(dfy_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(infl_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(svar_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(de_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(lty_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(tms_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(tbl_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(dfr_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(dp_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(dy_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(ltr_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(ep_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(bm_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(ik_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(ntis_stat$dRMSE*100,digits=2),nsmall=2),
                   format(round(eqis_stat$dRMSE*100,digits=2),nsmall=2)
                  )
        )

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Set table1 variables as numeric data type                               ####
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..

    table1$IS_aR2 <- as.numeric(table1$IS_aR2)
    table1$OOS_aR2 <- as.numeric(table1$OOS_aR2)
    table1$dRMSE <- as.numeric(table1$dRMSE)
    
##  ............................................................................
##  Create table comparing results to G&W                                   ####
##  ............................................................................

    table1_sum <- tibble(table1$Variable, 
                         table1$IS_aR2, 
                         table1_2005_g_w$`IS_aR2*`,
                         table1$OOS_aR2,
                         table1_2005_g_w$`OOS_aR2*`,
                         table1$dRMSE,
                         table1_2005_g_w$`dRMSE*`
                         )

##  ............................................................................
##  Print table1 tibble in console                                          ####
##  ............................................................................

    table1

##  ............................................................................
##  Print table1 DT in viewer                                               ####
##  ............................................................................
    
    DT <- datatable(table1_sum, filter = 'top',list(paging = FALSE)) %>%
      
    DT 
    
##  ............................................................................
##  Print table1 formattable in viewer                                      ####
##  ............................................................................

    customGreen0 = "#DeF7E9"
    customGreen = "#71CA97"
    customRed = "#ff7f7f"
    
    formattable(table1_2005_g_w, align = c("l", rep("r", NCOL(table1) - 1)),
        list(`Variable*` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), `table1$OOS_aR2*` = color_bar("customRed"))
      )
    formattable(table1, align = c("l", rep("r", NCOL(table1) - 1)),
        list(`Variable` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), `table1$OOS_aR2` = color_bar("customRed"))
      )
    
##  ............................................................................
##  Export tables to LaTeX                                                  ####
##  ............................................................................

    table1_tex <- xtable(table1)
    print(table1_tex, include.rownames = FALSE)

#   ____________________________________________________________________________
#   END                                                                     ####
#   ____________________________________________________________________________