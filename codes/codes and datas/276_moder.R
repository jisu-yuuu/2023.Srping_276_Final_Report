rm(list = ls())
#rebalancing for moderate portfolio using maximum expected return


library(quadprog)  
library(quantmod)
library(PerformanceAnalytics)
library(robust)
library(fields)
library(tseries)
library(lubridate)
library(fredr)
library(NMOF)
library(ROI.plugin.glpk)
library(ROI)
library(ROI.plugin.quadprog)
library(PortfolioAnalytics)
library(dplyr)

#________(get data from Yahoo Finance)________________

tickers = c("AAPL","CSCO","GOOG","WST","META","MSFT","MSI","SCX","ADBE",
            "NFLX","CRM","DXCM","FICO","PODD")

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
write.csv(rt_daily, file = "rt_daily_mod.csv", row.names=F) 

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

port_ret_select <- data.frame(matrix(0,99,100))


fee_rate <- 0.001
port_return_rebal<- rep(0, 100)
port_return_new<- rep(0, 100)

port_val <- rep(0, 100)
port_val_rebel <- rep(0, 100)
port_val_new <- rep(0, 100)
port_val[1] <- c(10^7)*(1-fee_rate)

fees <- c(10^7) *fee_rate
month_fee <- matrix(0,100,length(tickers))
month_fee[1] <- fees

#___________(2015.1.1 invest)____________________________________
# Obtaining stock data for the first selection
period_end <- ymd(start_date) %m+% months(6)
rt_daily_init<- subset(rt_daily, date>= start_date & date< period_end)
rt_daily_init <- subset(rt_daily_init, select=tickers)

sampCovRobMCD <- covRob(rt_daily_init, estim = "mcd", quan = .95, ntrial = 10000)
mu <- sampCovRobMCD$center
varcov <- sampCovRobMCD$cov

port_order = data.frame(std = sqrt(diag(varcov)), mu)
port_order = port_order[order(port_order$mu, decreasing=TRUE),]


port_select = head(port_order,7)
ticker_select <- rownames(port_select)
# using 7 stocks ------------------------------------
rt_daily_init <- subset(rt_daily_init, select=ticker_select)
sampCovRobMCD <- covRob(rt_daily_init, estim = "mcd", quan = .95, ntrial = 10000)
mu <- sampCovRobMCD$center
varcov <- sampCovRobMCD$cov
port_order_0 = data.frame(std = sqrt(diag(varcov)), mu)


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

port.std_0=port.std
port.ret_0=port.ret


#__________(using Max Sharp)________________

# Create Portfolio object
max_exp_return_portfolio <- PortfolioAnalytics::portfolio.spec(assets = rownames(port_select))

# Add the full investment constraint that specifies the weights must sum to 1
max_exp_return_portfolio <- PortfolioAnalytics::add.constraint(
  portfolio = max_exp_return_portfolio,
  type = "full_investment"
)
# Add the box constraint that ensure the weights are between 0.1 and 0.8
max_exp_return_portfolio <- PortfolioAnalytics::add.constraint(
  portfolio = max_exp_return_portfolio,
  type = "box", min = 0.05, max = 0.6
)

# Add objective to maximize mean returns
max_exp_return_portfolio <- PortfolioAnalytics::add.objective(
  portfolio = max_exp_return_portfolio,
  type = "return",
  name = "mean"
)

# Run the optimization
global_max_portfolio <- PortfolioAnalytics::optimize.portfolio(
  R = rt_daily_init,
  portfolio = max_exp_return_portfolio,
  optimize_method = "glpk",
  trace = TRUE
)

# Examine returned portfolio list object
weight <- global_max_portfolio$weights

ticker_selected[1] <- ticker_select
port.wts <- matrix(0, 100, length(ticker_select))

mu_0= sum(mu*weight)
std_0 = sqrt(weight %*% varcov %*% weight)

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
  
  sampCovRobMCD <- covRob(rt_daily_select, estim = "mcd", quan = .95, ntrial = 10000)
  mu <- sampCovRobMCD$center
  varcov <- sampCovRobMCD$cov
  
  port_order_rebal = data.frame(std = sqrt(diag(varcov)), mu)
  
  
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
  
  
  # Run the optimization
  global_max_portfolio <- PortfolioAnalytics::optimize.portfolio(
    R = rt_daily_select,
    portfolio = max_exp_return_portfolio,
    optimize_method = "glpk",
    trace = TRUE
  )
  
  # Examine returned portfolio list object
  weight <- global_max_portfolio$weights
  mu_rebal= sum(mu*weight)
  std_rebal = sqrt(weight %*% varcov %*% weight)
  
  # Calculate weights for minimum variance portfolio
  port.wts2 <- matrix(0, 100, length(ticker_select))
  
  ticker_select_rebal <- ticker_select
  port.wts2 <- matrix(0, 100, length(ticker_select_rebal))
  
  
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
  
  port_order = data.frame(std = sqrt(diag(varcov)), mu)
  port_order = port_order[order(port_order$mu, decreasing=TRUE),]
  
  port_select_new = head(port_order,7)
  ticker_select2 <- rownames(port_select_new)
  
  # using 7 stocks ------------------------------------
  rt_daily_init <- subset(rt_daily_init, select=ticker_select2)
  sampCovRobMCD <- covRob(rt_daily_init, estim = "mcd", quan = .95, ntrial = 10000)
  
  mu <- sampCovRobMCD$center
  varcov <- sampCovRobMCD$cov
  port_order_new = data.frame(std = sqrt(diag(varcov)), mu)
  
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
  
  
  # Run the optimization
  global_max_portfolio <- PortfolioAnalytics::optimize.portfolio(
    R = rt_daily_init,
    portfolio = max_exp_return_portfolio,
    optimize_method = "glpk",
    trace = TRUE
  )
  
  # Examine returned portfolio list object
  weight <- global_max_portfolio$weights
  
  mu_new= sum(mu*weight)
  std_new = sqrt(weight %*% varcov %*% weight)
  
  ticker_select_new <- ticker_select2
  port.wts3 <- matrix(0, 100, length(ticker_select2))
  
  port.wts3[1+j,] <- weight
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
    assign(paste0("port.ret_", j), port.ret_new)
    assign(paste0("port.std_", j), port.std_new)
    #assign(paste0("port_order_", j), port_order_new)
    assign(paste0("mu_", j), mu_new)
    assign(paste0("std_", j), std_new)
    ticker_selected[1+j] <- ticker_select_new
    ticker_select <- ticker_select_new
    month_fee[1+j] <- new_fee_new
  } else{
    port_val[1+j] <- port_val_rebel[1+j] + port_val[j]
    old_port.wts[,2+j] <- rebal_port.wts[,1+j]
    rt_ptf[1+j] <- rt_ptf_rebal[1+j]
    #assign(paste0("port_order_", j), port_order_rebal)
    assign(paste0("port.ret_", j), port.ret_rebal)
    assign(paste0("port.std_", j), port.std_rebal)
    assign(paste0("mu_", j), mu_rebal)
    assign(paste0("std_", j), std_rebal)
    ticker_selected[1+j] <- ticker_select_rebal
    ticker_select <- ticker_select_rebal
    month_fee[1+j] <- rebal_fee_rebal
  }
}

#make a cash at the end of the day
month_fee[72] <- month_fee[72] + port_val[72] * (fee_rate)
port_val[72] <- port_val[72] - (port_val[72] * (fee_rate))


write.csv(old_port.wts, file = "portfolio_weights_months_mod.csv", row.names=F) 
write.csv(rt_ptf, file = "return_portfolio_months_mod.csv", row.names=F)
write.csv(port_val, file = "port_val_months_mod.csv", row.names=F)
write.csv(ticker_selected, file = "tickers_mod.csv", row.names=F)
write.csv(month_fee, file = "fee_mod.csv", row.names=F)


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
