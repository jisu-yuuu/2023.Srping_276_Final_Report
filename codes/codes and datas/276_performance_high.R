library(quadprog)  
library(quantmod)
library(PerformanceAnalytics)
library(robust)
library(fields)
library(lubridate)
library(dplyr)
library(PortfolioAnalytics)
library(tseries)
library(dplyr)
library(tidyverse)
library(fredr)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

rm(list = ls())
# _______________Performance Analysis______________________


#________(get data from Yahoo Finance)________________
rt_daily = read.csv(file = "rt_daily_high.csv")
wts_ptf_monthly = read.csv("portfolio_weights_months_high.csv")
tickers = wts_ptf_monthly$tickers


rt_daily_real = subset(rt_daily, date>= "2015-01-01" & date< "2020-12-31")
time_period = length(index(rt_daily_real))

#__________ Plotting the Portfolio Weight ______________________
par(mfrow = c(3,2))
for (i in 1:6){
  port.wts = wts_ptf_monthly[,(12*i-10):(1+12*i)]
  nvar = nrow(port.wts)
  colnames(port.wts) = c(1, 2, 3, 4, 5, 6,7, 8, 9, 10, 11, 12)
  
  barplot(main = paste(as.character(2014+i), "Portfolio Weights"), t(t(port.wts+0.015)), col=rainbow(nrow(port.wts)), xlab = "months", ylim = c(0, 1), beside=TRUE)
  #legend("right", legend = wts_ptf_monthly$tickers, fill = rainbow(nvar), ncol = 1,cex = 0.2)
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('top',legend = wts_ptf_monthly$tickers, col = rainbow(nvar), lwd = 4, xpd = TRUE, horiz = TRUE, cex = 0.5, seg.len=0.1, bty = 'n')
legend('bottom',legend = wts_ptf_monthly$tickers, col = rainbow(nvar), lwd = 4, xpd = TRUE, horiz = TRUE, cex = 0.5, seg.len=0.1, bty = 'n')

#__________ Getting  Daily portfolio return _____________________________


init_month = month(rt_daily_real$date[1])
wts_ptf_initial = wts_ptf_monthly[,2]


count_month = c(init_month)
wts_ptf_daily = NULL

#__________ 1. Getting  Daily Portfolio Weight ________________________
for(i in 1:time_period){
  current_date = rt_daily_real$date[i]
  #_Check difference in month
  current_month = count_month[length(count_month)]
  new_month = month(current_date)
  if (new_month != current_month){
    count_month = c(count_month, new_month)
  }
  month_position = length(count_month)
  
  current_weight = wts_ptf_monthly[,1+month_position]
  
  #current_daily_return = rt_daily_xts[i, ]
  
  #current_rt_ptf = log(1+sum(current_weight*(exp(current_daily_return)-1)))
  
  #_Create xts data
  date = as.Date(current_date)
  new_xts = NULL
  for (i in 1:length(tickers)){
    new_xts = cbind(new_xts, xts(x = current_weight[i], order.by = c(date)))
  }
  for (i in 1:length(colnames(new_xts))){colnames(new_xts)[i] = tickers[i]}
  
  #_add this to wts_ptf_daily
  wts_ptf_daily = rbind(wts_ptf_daily, new_xts)
}

#__________ 2. Getting  Daily Portfolio Log Return ________________________
rt_ptf_daily = NULL

for(i in 1:time_period){
  current_date = rt_daily_real$date[i]
  current_weight = wts_ptf_daily[i,]
  rt_daily_real_return <- subset(rt_daily_real, select=tickers)
  ## Use formula: portfolio log return = log(sum(w_i * exp(r_i))) where r_i is indivial log return
  current_ptf_log_return = log(sum(as.numeric(wts_ptf_daily[i,])*exp(rt_daily_real_return[i,])))
  
  
  #_Create xts data
  date = as.Date(current_date)
  new_xts =  xts(x = current_ptf_log_return, order.by = date)
  colnames(new_xts) = "ptf_log_return"
  
  #_add this to wts_ptf_daily
  rt_ptf_daily = rbind(rt_ptf_daily, new_xts)
}

#__________ 3. Getting  Daily Portfolio Value (Without Fee; For Performance Approximation Only) ________________________
val_ptf_daily_no_fee = NULL 
for(i in 1:time_period){
  current_date = rt_daily_real$date[i]
  ## Use formula: portfolio log return = log(sum(w_i * exp(r_i))) where r_i is indivial log return
  x = rt_ptf_daily[1:i]
  #x = x[!x==-Inf]
  current_val_ptf_daily_no_fee = exp(sum(x))
  
  #_Create xts data
  date = as.Date(current_date)
  new_xts =  xts(x = current_val_ptf_daily_no_fee, order.by = date)
  colnames(new_xts) = "no fee value (unit)"
  
  #_add this to val_ptf_daily_no_fee
  val_ptf_daily_no_fee = rbind(val_ptf_daily_no_fee, new_xts)
}

#__________ 4. Visualizing  Daily portfolio return _____________________________

#__________ 4.1 Plotting Daily Portfolio Value _____________________________
plot(val_ptf_daily_no_fee,main="High Portfolio Value (Fee Excluded)",col = rgb(0,0,0), legend = "asdf", ylab = "unit price")

#__________ 4.2 Plotting Daily Asset Value _____________________________
#Generate random color
color_list = NULL
for (i in 1:length(tickers)){color_list = c(color_list, rgb(runif(1),runif(1), runif(1)))}
color_list = rainbow(length(tickers))
for (asset in tickers){
  rt_asset_daily =  rt_daily_real[,asset]
  val_asset_daily = NULL
  
  for(i in 1:time_period){
    current_date = rt_daily_real$date[i]
    
    ## Use formula: portfolio log return = log(sum(w_i * exp(r_i))) where r_i is indivial log return
    current_val_asset_daily = exp(sum(rt_asset_daily[1:i]))
    
    #_Create xts data
    date = as.Date(current_date)
    new_xts =  xts(x = current_val_asset_daily, order.by = date)
    colnames(new_xts) = "value (unit)"
    
    #_add this to val_ben_daily
    val_asset_daily = rbind(val_asset_daily, new_xts)
  }
  lines(val_asset_daily, col = color_list[which(tickers == asset)])
}
#_____________ 4.3 Adding a Legend _______________________________

xts::addLegend("topleft", on=1, 
               legend.names = tickers, 
               lty=c(1, 1), lwd=c(2, 1),
               col=color_list, cex = 0.7)

#__________ 5. Bench-mark-Related Performance _____________________________
# Storing the prices in one array
start_date <- "2015-01-01"
end_date <- "2020-12-31"
rt_SP_daily <- Ad(getSymbols("^GSPC", from = as.Date(start_date) %m-% months(12) , to = as.Date(end_date), 
                             auto.assign = FALSE))

# Calculate daily returns
rt_SP_daily <- na.omit(Return.calculate(rt_SP_daily, method = "log"))

rt_SP_daily<-as.data.frame(t(rt_SP_daily))
name_date<-colnames(rt_SP_daily)
rt_SP_daily<-as.data.frame(t(rt_SP_daily))
rt_SP_daily['date'] <- name_date # Added a date
rt_SP_daily$date <- as.Date(rt_SP_daily$date)
rt_SP_daily_real = subset(rt_SP_daily, select=colnames(rt_SP_daily)[1])
rt_SP_daily_real = rt_SP_daily_real[1:nrow(rt_SP_daily_real),]
#__________ 5.1 SP500 Value Plotting _____________________________

val_SP_daily = NULL

for(i in 1:time_period){
  current_date = rt_daily_real$date[i]
  rt_SP_daily_select<- subset(rt_SP_daily, date>= start_date) #OK
  rt_SP_daily_select <- subset(rt_SP_daily_select, select=colnames(rt_SP_daily)[1])
  
  
  ## Use formula: portfolio log return = log(sum(w_i * exp(r_i))) where r_i is indivial log return
  current_val_asset_daily = exp(sum(rt_SP_daily_select[1:i,])) #Data frame
  
  #_Create xts data
  date = as.Date(current_date)
  new_xts =  xts(x = current_val_asset_daily, order.by = date)
  colnames(new_xts) = "value (unit)"
  
  #_add this to val_ben_daily
  val_SP_daily = rbind(val_SP_daily, new_xts)}

plot(val_ptf_daily_no_fee,main="High Portfolio Value against SP500",col = rgb(0,0,0), legend = "asdf", ylab = "unit price")
lines(val_SP_daily, col = "red", lty = 2, lwd = 2)
xts::addLegend("topleft", on=1, 
               legend.names = c("Portfolio", "SP500"), 
               lty=c(1, 2), lwd=c(1, 1),
               col=c("black","red" ), cex = 0.7)


#__________ 5.2 SP500 Standard Deviation: 60-day _____________________________

windows = 60
start_index = which(year(rt_SP_daily$date) == 2015)[1]
time_index = start_index

rt_std_SP_daily = NULL
rt_mean_SP_daily = NULL
for (i in start_index:nrow(rt_SP_daily)){
  rt_SP_windowed = rt_SP_daily_real[(i-windows):(i-1)]
  rt_std = sd(rt_SP_windowed)
  rt_mean = mean(rt_SP_windowed)
  
  current_date = rt_SP_daily$date[i]
  date = as.Date(current_date)
  new_xts_sd =  xts(x = rt_std, order.by = date)
  new_xts_mean = xts(x = rt_mean, order.by = date)
  
  rt_std_SP_daily = rbind(rt_std_SP_daily, new_xts_sd)
  rt_mean_SP_daily = rbind(rt_mean_SP_daily, new_xts_mean)  
}
colnames(rt_std_SP_daily) = "std_SP_60_windows"
colnames(rt_mean_SP_daily) = "mean_SP_60_windows"

#plot(rt_std_SP_daily, main = "Portfolio Return against S.D of SP500")
lines(rt_mean_SP_daily + 8*rt_std_SP_daily, col = "red", lty = "dashed")
lines(rt_mean_SP_daily - 8*rt_std_SP_daily, col = "red", lty = "dashed")
plot(rt_ptf_daily,main = "Portfolio Return against S.D of SP500", col = "blue") #Portfolio return

#__________ 5.3 ULST _____________________________
# Storing the prices in one array
rt_ULST_daily <- Ad(getSymbols("ULST", from = as.Date(start_date) , to = as.Date(end_date), 
                               auto.assign = FALSE))

# Calculate daily returns
rt_ULST_daily <- na.omit(Return.calculate(rt_ULST_daily, method = "log"))

rt_ULST_daily<-as.data.frame(t(rt_ULST_daily))
name_date<-colnames(rt_ULST_daily)
rt_ULST_daily<-as.data.frame(t(rt_ULST_daily))
rt_ULST_daily['date'] <- name_date # Added a date
rt_ULST_daily$date <- as.Date(rt_ULST_daily$date)
rt_ULST_daily_real = subset(rt_ULST_daily, select=colnames(rt_ULST_daily)[1])
rt_ULST_daily_real = rt_ULST_daily_real[1:nrow(rt_ULST_daily_real),]
#__________ 5.3.1 ULST Value Plotting _____________________________

val_ULST_daily = NULL
rt_ULST_daily_select<- subset(rt_ULST_daily, date>= start_date) #OK
rt_ULST_daily_select <- subset(rt_ULST_daily_select, select=colnames(rt_ULST_daily)[1])
for(i in 1:time_period){
  current_date = rt_daily_real$date[i]
  
  
  
  ## Use formula: portfolio log return = log(sum(w_i * exp(r_i))) where r_i is indivial log return
  current_val_asset_daily = exp(sum(rt_ULST_daily_select[1:i,])) #Data frame
  
  #_Create xts data
  date = as.Date(current_date)
  new_xts =  xts(x = current_val_asset_daily, order.by = date)
  colnames(new_xts) = "value (unit)"
  
  #_add this to val_ben_daily
  val_ULST_daily = rbind(val_ULST_daily, new_xts)}
plot(val_ULST_daily, col = "green", lty = 1, lwd = 2)

#__________ 5.4 DIA _____________________________
# Storing the prices in one array
rt_DIA_daily <- Ad(getSymbols("DIA", from = as.Date(start_date) , to = as.Date(end_date), 
                              auto.assign = FALSE))

# Calculate daily returns
rt_DIA_daily <- na.omit(Return.calculate(rt_DIA_daily, method = "log"))

rt_DIA_daily<-as.data.frame(t(rt_DIA_daily))
name_date<-colnames(rt_DIA_daily)
rt_DIA_daily<-as.data.frame(t(rt_DIA_daily))
rt_DIA_daily['date'] <- name_date # Added a date
rt_DIA_daily$date <- as.Date(rt_DIA_daily$date)
rt_DIA_daily_real = subset(rt_DIA_daily, select=colnames(rt_DIA_daily)[1])
rt_DIA_daily_real = rt_DIA_daily_real[1:nrow(rt_DIA_daily_real),]
#__________ 5.3.1 ULST Value Plotting _____________________________

val_DIA_daily = NULL
rt_DIA_daily_select<- subset(rt_DIA_daily, date>= start_date) #OK
rt_DIA_daily_select <- subset(rt_DIA_daily_select, select=colnames(rt_DIA_daily)[1])
for(i in 1:time_period){
  current_date = rt_daily_real$date[i]
  
  
  
  ## Use formula: portfolio log return = log(sum(w_i * exp(r_i))) where r_i is indivial log return
  current_val_asset_daily = exp(sum(rt_DIA_daily_select[1:i,])) #Data frame
  
  #_Create xts data
  date = as.Date(current_date)
  new_xts =  xts(x = current_val_asset_daily, order.by = date)
  colnames(new_xts) = "value (unit)"
  
  #_add this to val_ben_daily
  val_DIA_daily = rbind(val_DIA_daily, new_xts)}

plot(val_ptf_daily_no_fee, col = "black", lty = 1, lwd = 2, main = "Portfolio values against benchmarks")
lines(val_ULST_daily, col = "green", lty = 2, lwd = 2)
lines(val_SP_daily, col = "red", lty = 2, lwd = 2)
lines(val_DIA_daily, col = "blue", lty = 2, lwd = 2)
xts::addLegend("topleft", on=1, 
               legend.names = c("Portfolio", "SP500", "DIA", "ULST"), 
               lty=c(1, 2,2,2), lwd=c(1, 1,1,1),
               col=c("black","red", "blue", "green" ), cex = 0.7)


lines(val_ptf_daily_no_fee, col = "brown", lty = 1, lwd = 2)


plot(cum_rt_CPIAUSCL, col = "black", lty = 2, lwd = 3)


###____________6. CPI Index Return ____________________________________
risk_level = 0.03
## ____________6.1. Get the cumulative CPI (similar to the wealth process)
fredr_set_key("b6649dc94ac869ef47073f922969b9bb")
CPIAUCSL <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("2014-12-31"),
  observation_end = as.Date("2020-12-31"))
# Make it into an xts file
CPIAUCSL <- xts(CPIAUCSL$value, order.by = CPIAUCSL$date)
rt_CPIAUCSL <- na.omit(Return.calculate(CPIAUCSL, method = "log"))
cum_rt_CPIAUSCL = rt_CPIAUCSL
for (i in 1:length(rt_CPIAUCSL)){cum_rt_CPIAUSCL[i] = exp(sum((rt_CPIAUCSL+risk_level)[1:i]))}
plot(cum_rt_CPIAUSCL)

## ____________6.2 .Get the portfolio cumulative log return (Both daily and monthly)
# Step 2.1. Get the cum daily return
cum_rt_ptf_daily = rt_ptf_daily 
for (i in 1:nrow(rt_ptf_daily)){cum_rt_ptf_daily[i,] = exp(sum(rt_ptf_daily[1:i,]))}

# Step 2.2. Get the cum monthly return
cum_rt_ptf_monthly = xts(x = cum_rt_ptf_daily[1], order.by = index(cum_rt_ptf_daily)[1])
for (i in 1:length(cum_rt_ptf_daily)){
  current_date = index(cum_rt_ptf_daily)[i]
  old_date = index(cum_rt_ptf_monthly)[length(cum_rt_ptf_monthly)]
  old_month = month(old_date)
  if (old_month!= month(current_date)){#different month then add entry
    new_xts = xts(x = cum_rt_ptf_daily[i], order.by = index(cum_rt_ptf_daily)[i])
    cum_rt_ptf_monthly = rbind(cum_rt_ptf_monthly, new_xts)
  }
  day(index(cum_rt_ptf_monthly)) = 1#For plotting purpose
}

# Get val_SP_daily above first
# Step 2A2. Calculate cum SP return monthly
cum_rt_SP_monthly = xts(x = val_SP_daily[1], order.by = index(val_SP_daily)[1])
for (i in 1:length(val_SP_daily)){
  current_date = index(val_SP_daily)[i]
  old_date = index(cum_rt_SP_monthly)[length(cum_rt_SP_monthly)]
  old_month = month(old_date)
  if (old_month!= month(current_date)){#different month then add entry
    new_xts = xts(x = val_SP_daily[i], order.by = index(val_SP_daily)[i])
    cum_rt_SP_monthly = rbind(cum_rt_SP_monthly, new_xts)
  }
  day(index(cum_rt_SP_monthly)) = 1 #For plotting purpose
}

plot(val_ptf_daily_no_fee, col = "black", lty = 1, lwd = 2, main = "Portfolio values against benchmarks")
plot(cum_rt_CPIAUSCL, col = "red", lty = "dashed", main = "Portfolio value against inflation (CPI)") 
lines(cum_rt_ptf_monthly, col = "black", lty = 1) 

xts::addLegend("topleft", on=1, 
               legend.names = c("Portfolio", "CPI"), 
               lty=c(1, 2), lwd=c(1, 1),
               col=c("black","red" ), cex = 0.7)


#lines(val_ptf_daily_no_fee,col = "blue", lty = "solid") 
lines(cum_rt_SP_monthly, col = "black", lty = "dashed") 
lines(cum_rt_ptf_monthly, col = "black", lty = 1) 
#lines(val_SP_daily, col = "red", lty = "solid") 

#################### Performance Analytics ############################################
#### NEED THE FOLLOWING VARIABLES ####
#rt_ptf: time series for portfolio return 
rt_ptf = rt_ptf_daily
#day:

## For Diversification Delta Only:
### NEED ALSO: rt_daily: xts file for the portfolio assets (each column is an asset)
#            rebal_per: rebalancing period
#            port.wts: Portfolio weights in a year
day = 60
### PA1. Empirical CDF ##############
unique_return =  sort(unique(rt_ptf))
n <- length(unique_return)
cum_prob = sapply(unique_return, function(x) sum(unique_return <= x) / n)
edf_manual <- data.frame(Returns = unique_return, CumulativeProbability = cum_prob)
plot(unique_return, cum_prob, type = "l", main = "Empirical Distribution Function (EDF) of Portfolio Returns",xlab = "Returns", ylab = "Cumulative Probability")

### PA2. Value at Risk (VaR) ############## 
VaR_daily <- NULL
# 20 day time window corresponding to 1 month
for (i in (day+1):nrow(rt_ptf)) {
  window_returns <- rt_ptf[(i-day+1):i]
  current_date = as.Date(index(rt_ptf)[i])
  new_xts = xts(x =VaR(window_returns, p = 0.95, method = "historical"), order.by = current_date )
  VaR_daily = rbind(VaR_daily, new_xts)
  #daily_VaR <- c(daily_VaR, VaR(window_returns, p = 0.95, method = "historical"))
}
plot(VaR_daily, type = "l", main = "Value at Risk (VaR) with 60-day window", ylab = "VaR", col="red")
xts::addLegend("left", on=1, 
               legend.names = "VaR", 
               lty=1, lwd=1,
               col="red")

lines(VaR_daily)
### PA3. Maximum Drawndown (MDD) ############## 
MDD_daily <- NULL
# 20 day time window corresponding to 1 month
for (i in (day+1):nrow(rt_ptf)) {
  window_returns <- rt_ptf[(i-day+1):i]
  current_date = as.Date(index(rt_ptf)[i])
  new_xts = xts(x =maxDrawdown(window_returns), order.by = current_date )
  MDD_daily = rbind(MDD_daily, new_xts)
  #daily_VaR <- c(daily_VaR, VaR(window_returns, p = 0.95, method = "historical"))
}

plot(MDD_daily, type = "l", main = "Maximum Draw-Down with 60-day window", ylab = "MDD")
xts::addLegend("topleft", on=1, 
               legend.names = "MDD", 
               lty=1, lwd=1,
               col="black")

### PA4. Portfolio sd, Sharpe Ratio: return/risk for 60-day window
rf = 0
mean_ptf_daily = NULL
sd_ptf_daily = NULL
Sharpe_ptf_daily = NULL

for (i in (day+1):nrow(rt_ptf)) {
  window_returns <- rt_ptf[(i-day+1):i]
  current_date = as.Date(index(rt_ptf)[i])
  mean_ptf_daily = rbind(mean_ptf_daily, xts(x =rt_ptf[i], order.by = current_date ))
  sd_ptf_daily = rbind(sd_ptf_daily, xts(x =sd(window_returns), order.by = current_date ))
  Sharpe_ptf_daily = rbind(Sharpe_ptf_daily, xts(x =(mean(window_returns)-rf)/sd(window_returns), order.by = current_date ))
  #daily_VaR <- c(daily_VaR, VaR(window_returns, p = 0.95, method = "historical"))
}

plot(sd_ptf_daily, main = "Portfolio standard deviation over 60-day window")
plot(Sharpe_ptf_daily, main = "Portfolio Sharpe Ratio over 60-day window")
lines(mean_ptf_daily, col = "blue")
lines(sd_ptf_daily, col ="red")

if (FALSE) { #Alternative Code
  for (i in 1:length(index(wts_ptf_daily))){
    current_date = index(wts_ptf_daily)[i]
    current_index = which(rt_daily$date == current_date)
    start_index = current_index - day+1
    rt_daily_windows = rt_daily[start_index:current_index,]
    rt_daily_windows = subset(rt_daily_windows, select = tickers)
    sampCovRobMCD <- covRob(rt_daily_windows, estim = "mcd", quan = .95, ntrial = 10000)
    varcov <- sampCovRobMCD$cov
    mu=sampCovRobMCD$center
    current_sd = sqrt(wts_ptf_daily[i,]%*% varcov %*%t(wts_ptf_daily[i,])) 
    current_mean = sum(mu*wts_ptf_daily[i,])
    
    mean_ptf_daily = rbind(mean_ptf_daily, xts(x = current_mean, order.by = current_date))
    sd_ptf_daily = rbind(sd_ptf_daily, xts(x = current_sd, order.by = current_date))
    Sharpe_ptf_daily = rbind(Sharpe_ptf_daily, xts(x = (current_mean--rf)/current_sd, order.by = current_date))
    print(i)
  }
}
day = 60
### PA7. Beta #################################
benchmark_prices <- Ad(getSymbols("^GSPC", from = "2014-12-31", to = "2020-12-31", 
                                  auto.assign = FALSE))
# Calculate daily returns
benchmark_returns <- na.omit(Return.calculate(benchmark_prices, method = "log"))

betas <- NULL
betasMCD <- NULL

for (i in (day+1):nrow(rt_ptf)) {
  window_returns <- rt_ptf[(i-day+1):i]
  window_rt_ben <- benchmark_returns[(i-day+1):i]
  current_date = as.Date(index(rt_ptf)[i])
  CovMCD <- covRob(na.omit(cbind(window_returns, window_rt_ben)), estim = "mcd", quan = .95, ntrial = 10000)
  current_betas = cov(window_returns, window_rt_ben)/(sd(window_rt_ben)^2) #Formula for beta
  current_betasMCD  = CovMCD$cov[1,2]/CovMCD$cov[2,2]
  betas = rbind(betas, xts(x = current_betas, order.by = current_date))
  betasMCD = rbind(betasMCD, xts(x = current_betasMCD, order.by = current_date))
  print(i)
  #daily_VaR <- c(daily_VaR, VaR(window_returns, p = 0.95, method = "historical"))
}
plot(betas, main = "Portfolio beta (computed in 2 ways)")
lines(betasMCD, col = "red", lty = 1)
xts::addLegend("bottomleft", on = 1, legend.names=c("Regular", "MCD"), col=c("black", "red"), lty=1)


### PA6. Diversification Delta (It depends on the portfolio weights)
### NEED ALSO: rt_daily: xts file for the portfolio assets (each column is an asset)
#            rebal_per: rebalancing period
#            port.wts: Portfolio weights in a year

#____________Getting Daily Tickers_
tickers_selected = read.csv("tickers_high.csv")
tickers_selected_daily = NULL
count_month = 1
for(i in 1:time_period){
  current_date = rt_daily_real$date[i]
  #_Check difference in month
  current_month = count_month[length(count_month)]
  new_month = month(current_date)
  if (new_month != current_month){
    count_month = c(count_month, new_month)
  }
  month_position = length(count_month)
  
  current_ticker = tickers_selected[,month_position]
  
  #current_daily_return = rt_daily_xts[i, ]
  
  #current_rt_ptf = log(1+sum(current_weight*(exp(current_daily_return)-1)))
  
  #_Create xts data
  date = as.Date(current_date)
  new_xts = xts(x = t(current_ticker), order.by = date)
  
  #_add this to wts_ptf_daily
  tickers_selected_daily = rbind(tickers_selected_daily, new_xts)
}

DivD = NULL
for (i in (day+1):nrow(rt_ptf)) {
  
  window_returns <- rt_ptf[(i-day+1):i]
  current_date = as.Date(index(rt_ptf)[i])
  assets_tickers = tickers_selected_daily[i]
  
  window_assets = subset(rt_daily , select = assets_tickers)
  var_ptf <- var(window_returns)
  var_assets <- diag(var(window_assets))
  entropy_ptf <- (1/2)*log(2*pi*var_ptf) + (1/2)
  entropy_assets <- (1/2)*log(2*pi*var_assets) + (1/2)
  current_weight = subset(wts_ptf_daily, select = assets_tickers)
  
  entropy_ats <- sum(current_weight[i,]*entropy_assets) #Need weight???
  DivD = rbind(DivD, xts(x = 1-exp(entropy_ptf - entropy_ats), order.by = current_date)) 
  
}
colnames(DivD) = "DivD"
plot(DivD, main = "Portfolio Diversification Delta over 60-day window", ylab = "Diversification Delta",
     ylim = c(-2,1))


  ### PA9. Treynor Ratio #################################
  ###### NEED TO USE THE Beta Ratio as well as the benchmark return
  benchmark_prices <- Ad(getSymbols("^GSPC", from = index(rt_ptf)[1], to = index(rt_ptf)[length(index(rt_ptf))], 
                                    auto.assign = FALSE))
  
  # Calculate daily returns
  benchmark_returns <- na.omit(Return.calculate(benchmark_prices, method = "log"))
  
  rf <- 0
  treynor <- NULL
  for (i in 1:(nrow(rt_ptf) - 21)) {
    window_rt_ptf <- rt_ptf[i:(i + 19), ]
    window_rt_ben <- benchmark_returns[i:(i + 19), ]
    beta <- cov(window_rt_ptf, window_rt_ben)/(sd(window_rt_ben)^2)
    treynor <- c(treynor, (mean(window_rt_ptf) - rf)/beta)
  }
  plot(treynor, type = "l", main = "Treynor Coefficient over the last 20 days", ylab = "Treynor Coefficient")
  
  
