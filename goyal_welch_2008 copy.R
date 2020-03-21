library(data.table)
library(ggplot2)
library(lubridate)
library(dyn)
library(reshape2)

#monthly <- read.csv2("/home/christophj/Dropbox/FinanceIssues/ReturnPredictability/Data/monthly_2005.csv",na.strings="NaN", stringsAsFactors=FALSE)
#annual  <- read.csv2("/home/christophj/Dropbox/FinanceIssues/ReturnPredictability/Data/annual_2005.csv", na.strings="NaN", stringsAsFactors=FALSE)

monthly <- fread(file.path("/home/christophj/Dropbox/FinanceIssues/ReturnPredictability/Data/","monthly_2005.csv"))
annual <- fread(file.path("/home/christophj/Dropbox/FinanceIssues/ReturnPredictability/Data/","annual_2005.csv"))

monthly <- as.data.table(monthly)
annual  <- as.data.table(annual)
# Jake Klyn Update: The ymd() function needs a character date string to be yyyymmdd. The "yyyymm" column
# does not have the dd part. I had to add "01" to the "yyyymm" column for the ymd() function to work. 
monthly <- monthly[, Datum := ymd(paste0(yyyymm,"01"))]

annual <- annual[, IndexDiv := Index + D12]
annual <- annual[, dp := log(D12) - log(Index)]
annual <- annual[, ep := log(E12) - log(Index)]
vec_dy <- c(NA, annual[2:nrow(annual), log(D12)] - annual[1:(nrow(annual)-1), log(Index)])
annual <- annual[, dy := vec_dy]
annual <- annual[, logret   :=c(NA,diff(log(Index)))]
vec_logretdiv <- c(NA, annual[2:nrow(annual), log(IndexDiv)] - annual[1:(nrow(annual)-1), log(Index)])
vec_logretdiv <- c(NA, log(annual[2:nrow(annual), IndexDiv]/annual[1:(nrow(annual)-1), Index]))
annual <- annual[, logretdiv:=vec_logretdiv]
annual <- annual[, logRfree := log(Rfree + 1)]
annual <- annual[, rp_div   := logretdiv - logRfree]
#Put it in time series (is needed in function get_statistics)
# Jake Klyn Update: Updated script to pull from the "yyyy" column, instead of Datum. (There was no Datum column in the CSV files)
ts_annual <- ts(annual, start=annual[1, yyyy], end=annual[nrow(annual), yyyy])
plot(ts_annual[, c("rp_div", "dp", "dy")])

#Define function

get_statistics <- function(ts_df, indep, dep, h=1, start=1872, end=2005, est_periods_OOS = 20) {
  #### IS ANALYSIS
  #1. Historical mean model
  avg   <- mean(window(ts_df, start, end)[, dep], na.rm=TRUE)
  IS_error_N <- (window(ts_df, start, end)[, dep] - avg)
  #2. OLS model
  # Jake Klyn Update: Overrode the lag() function to pull from the "stats" package (You might have been pulling the 
  # lag() function from "dplyr", which was probably why it was erring out for you.)
  reg <- dyn$lm(eval(parse(text=dep)) ~ stats::lag(eval(parse(text=indep)), -1), data=window(ts_df, start, end))
  IS_error_A <- reg$residuals
  ### 
  ####OOS ANALYSIS
  OOS_error_N <- numeric(end - start - est_periods_OOS)
  OOS_error_A <- numeric(end - start - est_periods_OOS)
  #Only use information that is available up to the time at which the forecast is made
  j <- 0
  for (i in (start + est_periods_OOS):(end-1)) {
    j <- j + 1
    #Get the actual ERP that you want to predict
    actual_ERP <- as.numeric(window(ts_df, i+1, i+1)[, dep])
    #1. Historical mean model
    OOS_error_N[j] <- actual_ERP - mean(window(ts_df, start, i)[, dep], na.rm=TRUE)
    #2. OLS model
    # Jake Klyn Update: Overrode the lag() function to pull from the "stats" package (You might have been pulling the 
    # lag() function from "dplyr", which was probably why it was erring out for you.)
    reg_OOS <- dyn$lm(eval(parse(text=dep)) ~ stats::lag(eval(parse(text=indep)), -1), 
                      data=window(ts_df, start, i))
    #Compute_error
    df <- data.frame(x=as.numeric(window(ts_df, i, i)[, indep]))
    names(df) <- indep
    pred_ERP   <- predict.lm(reg_OOS, newdata=df)
    OOS_error_A[j] <-  pred_ERP - actual_ERP
  }
  #Compute statistics 
  MSE_N <- mean(OOS_error_N^2)
  MSE_A <- mean(OOS_error_A^2)
  T <- length(!is.na(ts_df[, dep]))
  OOS_R2  <- 1 - MSE_A/MSE_N
  #Is the -1 enough (maybe -2 needed because of lag)?
  OOS_oR2 <- OOS_R2 - (1-OOS_R2)*(reg$df.residual)/(T - 1) 
  dRMSE <- sqrt(MSE_N) - sqrt(MSE_A)
  ##
  #### CREATE PLOT
  IS  <- cumsum(IS_error_N[2:length(IS_error_N)]^2)-cumsum(IS_error_A^2)
  OOS <- cumsum(OOS_error_N^2)-cumsum(OOS_error_A^2)
  df  <- data.frame(x=seq.int(from=start + 1 + est_periods_OOS, to=end), 
                    IS=IS[(1 + est_periods_OOS):length(IS)], 
                    OOS=OOS) #Because you lose one observation due to the lag
  #Shift IS errors vertically, so that the IS line begins 
  # at zero on the date of first OOS prediction. (see Goyal/Welch (2008, p. 1465))
  df$IS <- df$IS - df$IS[1] 
  df  <- melt(df, id.var="x") 
  plotGG <- ggplot(df) + 
    geom_line(aes(x=x, y=value,color=variable)) + 
    geom_rect(data=data.frame(),#Needed by ggplot2, otherwise not transparent
              aes(xmin=1973, xmax=1975,ymin=-0.2,ymax=0.2), 
              fill='red',
              alpha=0.1) + 
    scale_y_continuous('Cumulative SSE Difference', limits=c(-0.2, 0.2)) + 
    scale_x_continuous('Year')
  ##
  return(list(IS_error_N = IS_error_N,
              IS_error_A = reg$residuals,
              OOS_error_N = OOS_error_N,
              OOS_error_A = OOS_error_A,
              IS_R2 = summary(reg)$r.squared, 
              IS_aR2 = summary(reg)$adj.r.squared, 
              OOS_R2  = OOS_R2,
              OOS_oR2 = OOS_oR2,
              dRMSE = dRMSE,
              plotGG = plotGG))
}

#Dividend-price ratio

dp_stat <- get_statistics(ts_annual, "dp", "rp_div", start=1872)
dp_stat$plotGG

#Dividend-yield

dy_stat <- get_statistics(ts_annual, "dy", "rp_div", start=1872)
dy_stat$plotGG

#Earnings-price ratio

ep_stat <- get_statistics(ts_annual, "ep", "rp_div", start=1872)
ep_stat$plotGG
