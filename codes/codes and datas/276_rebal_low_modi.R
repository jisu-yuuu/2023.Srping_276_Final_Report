#rm(list = ls())
# LOW PORTFOLIO - GMV to choose 7 highest mean returns, and then calculate GMV for weights
# for moderate and high, need to change 7-asset selection part

library(quadprog)  
library(quantmod)
library(PerformanceAnalytics)
library(robust)
library(fields)
library(tseries)
library(lubridate)
library(tidyverse)
library(dplyr) 

#________(get data from Yahoo Finance)________________

tickers = c("GLD","IAU","SGOL","XLU","VPU","IDU","RYU","FXU","XLP","VDC","KXI")
start_date <- "2014-07-01"
end_date <- "2020-12-31"
df_Assets <- getSymbols(tickers, from = start_date, to = end_date)

# Storing the prices in one array
prices <- purrr::map(tickers, function(x) Ad(get(x)))
prices <- purrr::reduce(prices, merge)
colnames(prices) <- tickers

# Computing daily returns
rt_daily <- na.omit(Return.calculate(prices, method = "log"))
rt_daily<-as.data.frame(t(rt_daily))
name_date<-colnames(rt_daily)
rt_daily<-as.data.frame(t(rt_daily))
rt_daily['date'] <- name_date
rt_daily$date <- as.Date(rt_daily$date)

write.csv(rt_daily, file = "rt_daily.csv", row.names=F) 


set.seed(1)

# make data frame and matrix we will use
rebal_per <- 1
diff_in_months <- (year(ymd(end_date))  - year(ymd(start_date))) * 12 +
  (month(ymd(end_date)) - month(ymd(start_date))) - 6
rebal_freq <- diff_in_months/rebal_per

ticker_selected <- data.frame(matrix(0,7,100))

old_port.wts <- data.frame(matrix(0,length(tickers),100))
colnames(old_port.wts)[1]<-c('tickers')
old_port.wts[,1] <- c(tickers)

rebal_port.wts <- data.frame(matrix(0,length(tickers),100))
colnames(rebal_port.wts)[1]<-c('tickers')
rebal_port.wts[,1] <- c(tickers)

new_port.wts <- data.frame(matrix(0,length(tickers),100))
colnames(new_port.wts)[1]<-c('tickers')
new_port.wts[,1] <- c(tickers)

mu_select <- data.frame(matrix(0,length(tickers),100))
colnames(mu_select)[1]<-c('tickers')
mu_select[,1] <- c(tickers)

std_select <- data.frame(matrix(0,length(tickers),100))
colnames(std_select)[1]<-c('tickers')
std_select[,1] <- c(tickers)

fee_rate <- 0.001
port_return_rebal<- rep(0, 100)
port_return_new<- rep(0, 100)

port_val <- rep(0, 100)
port_val_rebel <- rep(0, 100)
port_val_new <- rep(0, 100)
port_val[1] <- c(10^7)*(1-fee_rate)

fees <- c(10^7) *fee_rate
month_fee <- matrix(0,1,length(tickers))
month_fee[1] <- fees

port.ret_sel <- data.frame(matrix(0,99,73))
port.std_sel <- data.frame(matrix(0,99,73))
mu_sel <- data.frame(matrix(0,7,73))
std_sel <- data.frame(matrix(0,7,73))



#___________(2015.1.1 invest)____________________________________
# Obtaining stock data for the first selection
period_end <- ymd(start_date) %m+% months(6)
rt_daily_init<- subset(rt_daily, date>= start_date & date< period_end)
rt_daily_init <- subset(rt_daily_init, select=tickers)

sampCovRobMCD <- covRob(rt_daily_init, estim = "mcd", quan = .95, ntrial = 10000)
mu <- sampCovRobMCD$center
varcov <- sampCovRobMCD$cov

# Reordering stocks based on returns
GMV_plots = data.frame(std = sqrt(diag(varcov)), mu)
GMV_plots = GMV_plots[order(GMV_plots$std, decreasing=FALSE),]

# Select top 7 stocks based on highest mean returns
port_select = head(GMV_plots,7)

# Finding the min_variance portfolio
ticker_select <- rownames(port_select)
rt_daily_init <- subset(rt_daily_init, select=ticker_select)
rt_daily_init <- as.matrix(rt_daily_init)

sampCovRobMCD <- covRob(rt_daily_init, estim = "mcd", quan = .95, ntrial = 10000)
mu <- sampCovRobMCD$center
varcov <- sampCovRobMCD$cov
port_order_0 = data.frame(std = sqrt(diag(varcov)), mu)
#mu_sel[1] <- port_order_0$mu
#std_sel[1] <- port_order_0$std

#Efficient Frontier
options(scipen = 999)
# Number of assets
nvar <- 7

# Number of efficient frontier points
n.er <- 100  
rset <- seq(min(mu), max(mu), length=n.er+2)
rset <- rset[2:n.er+1]

# Initialize variables to store results
port.ret <- rset
port.std <- rset*0
port.wgt <- matrix(0, n.er, nvar)

# Calculate efficient frontier using quadratic programming
for (i in 1:(n.er-1)) {
  Dmat <- 2*varcov
  dvec <- rep(0, nvar) 
  Amat <- t(rbind(t(rep(1, nvar)), t(mu), diag(nvar)))
  bvec <- c(1, rset[i], rep(0, nvar))
  
  # Mean-variance optimization
  m <- solve.QP(Dmat, dvec, Amat, bvec, meq = 2)
  
  # Store results
  port.std[i]  <- sqrt(m$value)
  port.wgt[i,] <- t(m$solution)
}


#port.ret_sel[1] <- port.std
#port.std_sel[1] <- port.ret
port.std_0=port.std
port.ret_0=port.ret


min_variance <- tseries::portfolio.optim(rt_daily_init, reshigh = rep(1,7), 
                                         reslow = rep(0,7),riskless=FALSE)

# Calculate weights for minimum variance portfolio
ticker_selected[1] <- ticker_select
port.wts <- matrix(0, 100, length(ticker_select))
weight <- min_variance$pw

mu_0= sum(mu*weight)
std_0 = sqrt(sum(weight %*% varcov %*% weight))

port.wts[2,] <- weight
port.wts<-as.data.frame(t(port.wts))
port.wts[,1]<- c(ticker_select)
names(port.wts) <- names(old_port.wts)
#sum(port.wts[,2]) # check for sum to 1

oldport <- full_join(old_port.wts, port.wts, by='tickers')
firstport=oldport[2:100]
secondport=oldport[101:199]
sumport=firstport+secondport
sumport=cbind(oldport[1],sumport)
sumport[is.na(sumport)] <- 0
old_port.wts[,2] <- sumport[,2]

#Return for first period
rt_ptf <- rep(0, 100) #portfolio return
rt_ptf_rebal <- rep(0, 100) #portfolio return for rebelancing case
rt_ptf_new <- rep(0, 100) #portfolio return for new slection case

period_end <- ymd(start_date) %m+% months(7)
prev_date <- ymd(start_date) %m+% months(6)
rt_daily_return <- subset(rt_daily, date>= prev_date & date< period_end)
rt_daily_return <- subset(rt_daily_return, select=tickers)

for (i in 1:nrow(rt_daily_return)){
  rt_ptf[1] = rt_ptf[1]+log(sum(old_port.wts[,2]*exp(rt_daily_return[i,])))
  }
port_val[2] <- port_val[1] + rt_ptf[1]*(10^7)

#for loop 
for (j in 1: rebal_freq){
  #For a new period_rebalencing
  period_end <- ymd(start_date) %m+% months(j+6)
  prev_date <- ymd(period_end) %m-% months(6)
  
  #Get daily return to 7 stocks
  rt_daily_select <- subset(rt_daily, date>= prev_date & date< period_end)
  rt_daily_select <- subset(rt_daily_select, select=ticker_select)
  
  # Computing the robust variance-covariance matrix
  sampCovRobMCD <- covRob(rt_daily_select, estim = "mcd", quan = .95, ntrial = 10000)
  varcov <- sampCovRobMCD$cov
  mu=sampCovRobMCD$center
  
  port_order_rebal = data.frame(std = sqrt(diag(varcov)), mu)
  #mu_rebal <- port_order_rebal$mu
  #std_rebal <- port_order_rebal$std
  #Efficient Frontier
  options(scipen = 999)
  # Number of assets
  nvar <- 7
  
  # Number of efficient frontier points
  n.er <- 100  
  rset <- seq(min(mu), max(mu), length=n.er+2)
  rset <- rset[2:n.er+1]
  
  # Initialize variables to store results
  port.ret <- rset
  port.std <- rset*0
  port.wgt <- matrix(0, n.er, nvar)
  
  # Calculate efficient frontier using quadratic programming
  for (i in 1:(n.er-1)) {
    Dmat <- 2*varcov
    dvec <- rep(0, nvar) 
    Amat <- t(rbind(t(rep(1, nvar)), t(mu), diag(nvar)))
    bvec <- c(1, rset[i], rep(0, nvar))
    
    # Mean-variance optimization
    m <- solve.QP(Dmat, dvec, Amat, bvec, meq = 2)
    
    # Store results
    port.std[i]  <- sqrt(m$value)
    port.wgt[i,] <- t(m$solution)
  }
  
  port.std_rebal=port.std
  port.ret_rebal=port.ret
  
  # Get the minimum variance portfolio
  
  min_variance <- tseries::portfolio.optim(rt_daily_init, reshigh = rep(1,7), 
                                           reslow = rep(0,7),riskless=FALSE)
  
  ticker_select_rebal <- ticker_select
  port.wts2 <- matrix(0, 100, length(ticker_select_rebal))
  weight <- min_variance $pw
  
  mu_rebal= sum(mu*weight)
  std_rebal = sqrt(weight %*% varcov %*% weight)
  
  
  port.wts2[1+j,] <- weight
  port.wts2<-as.data.frame(t(port.wts2))
  port.wts2[,1]<- c(ticker_select)
  names(port.wts2) <- names(old_port.wts)
  #sum(port.wts[,2]) # check for sum to 1
  
  oldport = full_join(rebal_port.wts,port.wts2,by='tickers')
  firstport=oldport[2:100]
  secondport=oldport[101:199]
  sumport=firstport+secondport
  sumport=cbind(oldport[1],sumport)
  sumport[is.na(sumport)] <- 0
  rebal_port.wts[,1+j] <- sumport[,1+j]
  
  #Find the new stock
  #period_end <- ymd(start_date) %m+% months(j+6)
  #prev_date <- ymd(start_date) %m+% months(j)
  rt_daily_init <- subset(rt_daily, date>= prev_date & date< period_end)
  rt_daily_init <- subset(rt_daily_init, select=tickers)
  
  sampCovRobMCD <- covRob(rt_daily_init, estim = "mcd", quan = .95, ntrial = 10000)
  mu <- sampCovRobMCD$center
  varcov <- sampCovRobMCD$cov

  # Reordering stocks based on returns
  GMV_plots = data.frame(std = sqrt(diag(varcov)), mu)
  GMV_plots = GMV_plots[order(GMV_plots$std, decreasing=FALSE),]
  
  # Select top 7 stocks based on highest mean returns
  port_select_new = head(GMV_plots,7)
  
    # Calculate weights for minimum variance portfolio
  ticker_select2 = rownames(port_select_new)
  ticker_select_new <- ticker_select2
  rt_daily_init <- subset(rt_daily_init, select=ticker_select2)
  rt_daily_init <- as.matrix(rt_daily_init)
  
  sampCovRobMCD <- covRob(rt_daily_init, estim = "mcd", quan = .95, ntrial = 10000)
  mu <- sampCovRobMCD$center
  varcov <- sampCovRobMCD$cov
  port_order_new = data.frame(var = sqrt(diag(varcov)), mu)
  #mu_new <- port_order_new$mu
  #std_new <- port_order_new$std
  #Efficient Frontier
  options(scipen = 999)
  # Number of assets
  nvar <- 7
  
  # Number of efficient frontier points
  n.er <- 100  
  rset <- seq(min(mu), max(mu), length=n.er+2)
  rset <- rset[2:n.er+1]
  
  # Initialize variables to store results
  port.ret <- rset
  port.std <- rset*0
  port.wgt <- matrix(0, n.er, nvar)
  
  # Calculate efficient frontier using quadratic programming
  for (i in 1:(n.er-1)) {
    Dmat <- 2*varcov
    dvec <- rep(0, nvar) 
    Amat <- t(rbind(t(rep(1, nvar)), t(mu), diag(nvar)))
    bvec <- c(1, rset[i], rep(0, nvar))
    
    # Mean-variance optimization
    m <- solve.QP(Dmat, dvec, Amat, bvec, meq = 2)
    
    # Store results
    port.std[i]  <- sqrt(m$value)
    port.wgt[i,] <- t(m$solution)
  }
  
  port.std_new=port.std
  port.ret_new=port.ret
  
  min_variance <- tseries::portfolio.optim(rt_daily_init, reshigh = rep(1,7), 
                                           reslow = rep(0,7),riskless=FALSE)
  
  port.wts3 <- matrix(0, 100, length(ticker_select))
  weight <-  min_variance$pw
  
  mu_new= sum(mu*weight)
  std_new = sqrt(weight %*% varcov %*% weight)
  
  port.wts3[2,] <-weight
  port.wts3<-as.data.frame(t(port.wts3))
  port.wts3[,1]<- c(ticker_select2)
  names(port.wts3)<-names(old_port.wts)
  #sum(port.wts3[,2]) # check for sum to 1
  
  oldport = full_join(new_port.wts,port.wts3,by='tickers')
  firstport=oldport[2:100]
  secondport=oldport[101:199]
  sumport=firstport+secondport
  sumport=cbind(oldport[1],sumport)
  sumport[is.na(sumport)] <- 0
  new_port.wts[,1+j] <- sumport[,1+j]

  #calculate return and fee for rebalancing.
  diff_port_rebal.wts = rebal_port.wts[,1+j] - old_port.wts[,1+j]
  rebal_fee<- diff_port_rebal.wts * fee_rate
  rebal_fee_rebal <- sum(abs(rebal_fee))
  port_val_rebel[1+j] <- port_val_rebel[1+j] - rebal_fee_rebal #put fee
  
  
  prev_date <- ymd(start_date) %m+% months(6+j)
  period_end <- ymd(start_date) %m+% months(7+j)
  rt_daily_return <- subset(rt_daily, date>= prev_date & date< period_end)
  rt_daily_return <- subset(rt_daily_return, select=tickers)
  

  for (i in 1:nrow(rt_daily_return)){
    rt_ptf_rebal[1+j]<- log(sum(rebal_port.wts[,1+j]*exp(rt_daily_return[i,]))) + rt_ptf_rebal[1+j]
    }
  port_val_rebel[1+j] <-rt_ptf_rebal[1+j] * port_val[j] + port_val_rebel[1+j]

  
  #find the value of new stocks
  diff_port_new.wts = new_port.wts[,1+j] - old_port.wts[,1+j]
  new_fee<- diff_port_new.wts * fee_rate
  new_fee_new <- sum(abs(new_fee))
  port_val_new[1+j] <- port_val_new[1+j] - new_fee_new
  
  for (i in 1:nrow(rt_daily_return)){
    rt_ptf_new[1+j]<- log(sum(new_port.wts[,1+j]*exp(rt_daily_return[i,]))) + rt_ptf_new[1+j]
  }
  port_val_new[1+j] <-rt_ptf_new[1+j] * port_val[j] + port_val_new[1+j]

  if (port_val_new[1+j] >= port_val_rebel[1+j]){
    port_val[1+j] <- port_val_new[1+j] + port_val[j]
    old_port.wts[,2+j] <- new_port.wts[,1+j]
    rt_ptf[1+j] <- rt_ptf_new[1+j]
    assign(paste0("mu_", j), mu_new)
    assign(paste0("std_", j), std_new)
    #port.ret_sel[1+j] <- port.ret_new
    #port.std_sel[1+j] <- port.std_new
    #mu_sel[1+j] <- mu_new
    #std_sel[1+j] <- std_new
    assign(paste0("port.ret_", j), port.ret_new)
    assign(paste0("port_order_", j), port_order_new)
    assign(paste0("port.std_", j), port.std_new)
    ticker_selected[1+j] <- ticker_select_new
    ticker_select <- ticker_select_new
    month_fee[1+j] <- new_fee_new
  } else{
    port_val[1+j] <- port_val_rebel[1+j] + port_val[j]
    old_port.wts[,2+j] <- rebal_port.wts[,1+j]
    rt_ptf[1+j] <- rt_ptf_rebal[1+j]
    assign(paste0("port.ret_", j), port.ret_rebal)    
    assign(paste0("mu_", j), mu_rebal)
    assign(paste0("std_", j), std_rebal)
    #port.ret_sel[1+j] <- port.ret_rebal
    #port.std_sel[1+j] <- port.std_rebal
    #mu_sel[1+j] <- mu_rebal
    #std_sel[1+j] <- std_rebal
    assign(paste0("port.std_", j), port.std_rebal)
    assign(paste0("port_order_", j), port_order_rebal)
    ticker_selected[1+j] <- ticker_select_rebal
    ticker_select <- ticker_select_rebal
    #colnames(port_select_new)
    month_fee[1+j] <- rebal_fee_rebal
  }
}

 #make a cash at the end of the day
month_fee[72] <- month_fee[72] + port_val[72] * (fee_rate)
port_val[72] <- port_val[72] - (port_val[72] * (fee_rate))

# port_ret_select <- port_ret_select[,c(1,3:100)] #remove empty column
write.csv(old_port.wts, file = "portfolio_weights_months.csv", row.names=F) 
write.csv(rt_ptf, file = "return_portfolio_months.csv", row.names=F)
write.csv(port_val, file = "port_val_months.csv", row.names=F)
write.csv(ticker_selected, file = "tickers.csv", row.names=F)
write.csv(month_fee, file = "fee.csv", row.names=F)
write.csv(rt_daily, file = "rt_daily.csv", row.names=F) 


# Plotting Efficient Frontier -----------------------------------------------
#2015 efficient frontier
color_list = c("bisque4","aquamarine4","brown4","chartreuse4",
               "coral4","cyan4","darkgoldenrod4","darkolivegreen4","darkorange4","darkorchid4",
               "darkseagreen","chocolate")
color_list = NULL
par(mfrow = c(3,2))
for (i in 0:11){color_list = c(color_list, rgb(runif(1), runif(1), runif(1)))}
all_ret = NULL
all_std = NULL
for (i in 0:11){
  y_values = get(paste("port.ret_",i, sep = ""))
  x_values = get(paste("port.std_",i, sep = ""))
  mup = get(paste("mu_",i, sep = ""))
  sigmap = get(paste("std_",i, sep = ""))
  all_ret = c(all_ret, y_values, mup)
  all_std = c(all_std, x_values, sigmap)
}
xmin = min(all_std)
xmax = 1.2*max(all_std)
ymin = 1.2*min(all_ret)
ymax = 1.2*max(all_ret)
plot(0, 0, ylim = c(ymin,ymax), xlim = c(xmin,xmax), col = "bisque4", lwd = 1,type="l",
     xlab = "Risk(Std)", ylab = "Return")
for (i in 0:11){
  mup = get(paste("mu_",i, sep = ""))
  sigmap = get(paste("std_",i, sep = ""))
  y_values = get(paste("port.ret_",i, sep = ""))
  x_values = get(paste("port.std_",i, sep = ""))
  lines(x_values, y_values, col = color_list[i], lwd = 2)
  points(sigmap, mup, col = color_list[i], lwd = 10)
}
legend(x='right', legend = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
                             "Oct","Nov","Dec"), col=color_list,lty=1:2, cex=0.8)

title('2015 Monthly Efficient Frontier')


for (i in 12:23){color_list = c(color_list, rgb(runif(1), runif(1), runif(1)))}
all_ret = NULL
all_std = NULL
for (i in 12:23){
  y_values = get(paste("port.ret_",i, sep = ""))
  x_values = get(paste("port.std_",i, sep = ""))
  mup = get(paste("mu_",i, sep = ""))
  sigmap = get(paste("std_",i, sep = ""))
  all_ret = c(all_ret, y_values, mup)
  all_std = c(all_std, x_values, sigmap)
}
xmin = min(all_std)
xmax = 1.2*max(all_std)
ymin = 1.2*min(all_ret)
ymax = 1.2*max(all_ret)
plot(0, 0, ylim = c(ymin,ymax), xlim = c(xmin,xmax), col = "bisque4", lwd = 1,type="l",
     xlab = "Risk(Std)", ylab = "Return")
for (i in 12:23){
  mup = get(paste("mu_",i, sep = ""))
  sigmap = get(paste("std_",i, sep = ""))
  y_values = get(paste("port.ret_",i, sep = ""))
  x_values = get(paste("port.std_",i, sep = ""))
  lines(x_values, y_values, col = color_list[i], lwd = 2)
  points(sigmap, mup, col = color_list[i], lwd = 10)
}
legend(x='right', legend = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
                             "Oct","Nov","Dec"), col=color_list,lty=1:2, cex=0.8)

title('2016 Monthly Efficient Frontier')

for (i in 24:35){color_list = c(color_list, rgb(runif(1), runif(1), runif(1)))}
all_ret = NULL
all_std = NULL
for (i in 24:35){
  y_values = get(paste("port.ret_",i, sep = ""))
  x_values = get(paste("port.std_",i, sep = ""))
  mup = get(paste("mu_",i, sep = ""))
  sigmap = get(paste("std_",i, sep = ""))
  all_ret = c(all_ret, y_values, mup)
  all_std = c(all_std, x_values, sigmap)
}
xmin = min(all_std)
xmax = 1.2*max(all_std)
ymin = 1.2*min(all_ret)
ymax = 1.2*max(all_ret)
plot(0, 0, ylim = c(ymin,ymax), xlim = c(xmin,xmax), col = "bisque4", lwd = 1,type="l",
     xlab = "Risk(Std)", ylab = "Return")
for (i in 24:35){
  mup = get(paste("mu_",i, sep = ""))
  sigmap = get(paste("std_",i, sep = ""))
  y_values = get(paste("port.ret_",i, sep = ""))
  x_values = get(paste("port.std_",i, sep = ""))
  lines(x_values, y_values, col = color_list[i], lwd = 2)
  points(sigmap, mup, col = color_list[i], lwd = 10)
}
legend(x='right', legend = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
                             "Oct","Nov","Dec"), col=color_list,lty=1:2, cex=0.8)

title('2017 Monthly Efficient Frontier')

for (i in 36:47){color_list = c(color_list, rgb(runif(1), runif(1), runif(1)))}
all_ret = NULL
all_std = NULL
for (i in 36:47){
  y_values = get(paste("port.ret_",i, sep = ""))
  x_values = get(paste("port.std_",i, sep = ""))
  mup = get(paste("mu_",i, sep = ""))
  sigmap = get(paste("std_",i, sep = ""))
  all_ret = c(all_ret, y_values, mup)
  all_std = c(all_std, x_values, sigmap)
}
xmin = min(all_std)
xmax = 1.2*max(all_std)
ymin = 1.2*min(all_ret)
ymax = 1.2*max(all_ret)
plot(0, 0, ylim = c(ymin,ymax), xlim = c(xmin,xmax), col = "bisque4", lwd = 1,type="l",
     xlab = "Risk(Std)", ylab = "Return")
for (i in 36:47){
  mup = get(paste("mu_",i, sep = ""))
  sigmap = get(paste("std_",i, sep = ""))
  y_values = get(paste("port.ret_",i, sep = ""))
  x_values = get(paste("port.std_",i, sep = ""))
  lines(x_values, y_values, col = color_list[i], lwd = 2)
  points(sigmap, mup, col = color_list[i], lwd = 10)
}
legend(x='right', legend = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
                             "Oct","Nov","Dec"), col=color_list,lty=1:2, cex=0.8)

title('2018 Monthly Efficient Frontier')

for (i in 48:59){color_list = c(color_list, rgb(runif(1), runif(1), runif(1)))}
all_ret = NULL
all_std = NULL
for (i in 48:59){
  y_values = get(paste("port.ret_",i, sep = ""))
  x_values = get(paste("port.std_",i, sep = ""))
  mup = get(paste("mu_",i, sep = ""))
  sigmap = get(paste("std_",i, sep = ""))
  all_ret = c(all_ret, y_values, mup)
  all_std = c(all_std, x_values, sigmap)
}
xmin = min(all_std)
xmax = 1.2*max(all_std)
ymin = 1.2*min(all_ret)
ymax = 1.2*max(all_ret)
plot(0, 0, ylim = c(ymin,ymax), xlim = c(xmin,xmax), col = "bisque4", lwd = 1,type="l",
     xlab = "Risk(Std)", ylab = "Return")
for (i in 48:59){
  mup = get(paste("mu_",i, sep = ""))
  sigmap = get(paste("std_",i, sep = ""))
  y_values = get(paste("port.ret_",i, sep = ""))
  x_values = get(paste("port.std_",i, sep = ""))
  lines(x_values, y_values, col = color_list[i], lwd = 2)
  points(sigmap, mup, col = color_list[i], lwd = 10)
}
legend(x='right', legend = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
                             "Oct","Nov","Dec"), col=color_list,lty=1:2, cex=0.8)

title('2019 Monthly Efficient Frontier')

for (i in 60:71){color_list = c(color_list, rgb(runif(1), runif(1), runif(1)))}
all_ret = NULL
all_std = NULL
for (i in 60:71){
  y_values = get(paste("port.ret_",i, sep = ""))
  x_values = get(paste("port.std_",i, sep = ""))
  mup = get(paste("mu_",i, sep = ""))
  sigmap = get(paste("std_",i, sep = ""))
  all_ret = c(all_ret, y_values, mup)
  all_std = c(all_std, x_values, sigmap)
}
xmin = min(all_std)
xmax = 1.2*max(all_std)
ymin = 1.2*min(all_ret)
ymax = 1.2*max(all_ret)
plot(0, 0, ylim = c(ymin,ymax), xlim = c(xmin,xmax), col = "bisque4", lwd = 1,type="l",
     xlab = "Risk(Std)", ylab = "Return")
for (i in 60:71){
  mup = get(paste("mu_",i, sep = ""))
  sigmap = get(paste("std_",i, sep = ""))
  y_values = get(paste("port.ret_",i, sep = ""))
  x_values = get(paste("port.std_",i, sep = ""))
  lines(x_values, y_values, col = color_list[i], lwd = 2)
  points(sigmap, mup, col = color_list[i], lwd = 10)
}
legend(x='right', legend = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
                             "Oct","Nov","Dec"), col=color_list,lty=1:2, cex=0.8)

title('2020 Monthly Efficient Frontier')