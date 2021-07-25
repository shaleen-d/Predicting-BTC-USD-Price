library(jsonlite)
library(TTR)
library(IDPmisc)
library(xts)  # For time series related calculations used by MASS
library(MASS)

## Get Data ----
# Data from JSON (investing.com)
# source("Json_parser.R")
# btc_data <- fromJSON("https://advcharts.investing.com/advinion2016/advanced-charts/56/56/23/GetRecentHistory?strSymbol=945629&iTop=1500&strPriceType=bid&strFieldsMode=allFields&strUserID=223062396&strExtraData=lang_ID=56&strTimeFrame=60M")
# btc_df <- parse_JSON(btc_data)

# Data from file
btc_df <- read.csv("BTCUSD_1hr.csv")

# Process date
btc_df$Date <- as.POSIXct(strptime(btc_df$Date, "%Y-%m-%d %H:%M:%S"))

# Exclude values likely to contain non-leading NAs
btc_df <- btc_df[1:22000, ]

## Functions to Mark turning points ----
# Return turning point or 1st order High for correction (bear market)
Turning_highs <- function(df, indices) {
  tp <- numeric(0)
  for(i in seq(2, length(indices)-1)) 
    if(df$Close[indices[i]] > df$Close[indices[i-1]] & 
       df$Close[indices[i]] > df$Close[indices[i+1]]) tp <- c(tp, indices[i])
    tp
}

# Return turning point or 1st order Low for rally (bull market)
Turning_lows <- function(df, indices) {
  tp <- numeric(0)
  for(i in seq(2, length(indices)-1)) 
    if(df$Close[indices[i]] < df$Close[indices[i-1]] & 
       df$Close[indices[i]] < df$Close[indices[i+1]]) tp <- c(tp, indices[i])
    tp
}

# Calculate K line and D line of Stochastic Oscillator
Calculate_stoch <- function(df) {
  stochOSC <- stoch( df[,c("High","Low","Close")] )
  df$fastK <- stochOSC[,"fastK"]
  df$slowD <- stochOSC[,"slowD"]
  # btc_df$adx <- ADX(btc_df[,c("High","Low","Close")])[,"ADX"]
  # btc_df$macd  <- MACD( btc_df[,"Close"], 12, 26, 9, maType="EMA" )[,"macd"]
  # btc_df$obv <- OBV(btc_df[,"Close"], btc_df[,"Volume"])
  
  df <- NaRV.omit(df)
  df
}

btc_df <- Calculate_stoch(btc_df)

## Mark Turning POints ----
# h1 <- Turning_highs(btc_df, 1:nrow(btc_df))
l1 <- Turning_lows(btc_df, 1:nrow(btc_df)) # 1st order lows
# h2 <- Turning_highs(btc_df, h1)
l2 <- Turning_lows(btc_df, l1) # 2nd order lows
# h3 <- Turning_highs(btc_df, h2)
# l3 <- Turning_lows(btc_df, l2)

# s1 <- sort(c(h1, l1))
# s2 <- sort(c(h2, l2))
# s3 <- sort(c(h3, l3))
btc_df$t_point <- 0
btc_df$t_point[l2] <- 1

## Compare models ----
model <- glm(
  t_point ~ fastK + slowD,
  family = "binomial", 
  data = btc_df[l1,]
)

# plot(btc_df$fastK[l1], btc_df$t_point[l1])
# hist(btc_df$fastK[btc_df$t_point == 1])

# Model of MACD
macd  <- MACD( btc_df[l1, "Close"], 12, 26, 9, maType="EMA" )
model_macd <- glm(
  btc_df[l1,]$t_point ~ macd,
  family = "binomial"
)

# Model of MFI
mfi <- MFI(btc_df[l1 ,c("High","Low","Close")], btc_df[l1 ,"Volume"])
model_mfi <- glm(
  btc_df[l1,]$t_point ~ mfi,
  family = "binomial"
)

# Model of RSI
rsi <- RSI(btc_df[l1,"Close"])
model_rsi <- glm(
  btc_df[l1,]$t_point ~ rsi,
  family = "binomial"
)

# Model of Stochastic
stochOSC <- stoch(btc_df[l1 ,c("High","Low","Close")])
model_fastk <- glm(
  btc_df[l1,]$t_point ~ stochOSC[,'fastK'],
  family = "binomial"
)

model_fastd <- glm(
  btc_df[l1,]$t_point ~ stochOSC[,'fastD'],
  family = "binomial"
)

model_slowd <- glm(
  btc_df[l1,]$t_point ~ stochOSC[,'slowD'],
  family = "binomial"
)

# Adding to fastK
model_fastk_rsi <- glm(
  btc_df[l1,]$t_point ~ stochOSC[,'fastK'] + rsi,
  family = "binomial"
)

model_fastk_rsi_mfi <- glm(
  btc_df[l1,]$t_point ~ stochOSC[,'fastK'] + rsi + mfi,
  family = "binomial"
)

model_fastk_rsi_fastd <- glm(
  btc_df[l1,]$t_point ~ stochOSC[,'fastK'] + rsi + stochOSC[,'fastD'],
  family = "binomial"
)

model_fastk_rsi_fastd_macd <- glm(
  btc_df[l1,]$t_point ~ stochOSC[,'fastK'] + rsi + stochOSC[,'fastD'] + macd,
  family = "binomial"
)

model_fastk_rsi_fastd_macd_slowd <- glm(
  btc_df[l1,]$t_point ~ stochOSC[,'fastK'] + rsi + stochOSC[,'fastD'] + macd + stochOSC[,'slowD'],
  family = "binomial"
)

model_fastk_rsi_fastd_slowd <- glm(
  btc_df[l1,]$t_point ~ stochOSC[,'fastK'] + rsi + stochOSC[,'fastD'] + stochOSC[,'slowD'],
  family = "binomial"
)

model_fastk_rsi_fastd_macd_slowd <- glm(
  btc_df[l1,]$t_point ~ stochOSC[,'fastK'] + rsi + stochOSC[,'fastD'] + macd[,'macd'] + stochOSC[,'slowD'],
  family = "binomial"
)

## Tuning Hyperparameters for chosen model ----
# fastK = 12
AIC_values <- numeric(0)
for(i in 10:20) {
  stochOSC <- stoch(btc_df[l1 ,c("High","Low","Close")], nFastK=i)
  model <- glm(
    btc_df[l1,]$t_point ~ stochOSC[,'fastK'] + rsi + stochOSC[,'fastD'] + macd[,'macd'] + stochOSC[,'slowD'],
    family = "binomial"
  )
  AIC_values <- c(AIC_values, model$aic)
}

# fastD = 2
AIC_values <- numeric(0)
for(i in 2:10) {
  stochOSC <- stoch(btc_df[l1 ,c("High","Low","Close")], nFastK = 12, nFastD=i)
  model <- glm(
    btc_df[l1,]$t_point ~ stochOSC[,'fastK'] + rsi + stochOSC[,'fastD'] + macd[,'macd'] + stochOSC[,'slowD'],
    family = "binomial"
  )
  AIC_values <- c(AIC_values, model$aic)
}

# slowD = 5
AIC_values <- numeric(0)
for(i in 2:10) {
  stochOSC <- stoch(btc_df[l1 ,c("High","Low","Close")], nFastK = 12, nFastD = 2, nSlowD=i)
  model <- glm(
    btc_df[l1,]$t_point ~ stochOSC[,'fastK'] + rsi + stochOSC[,'fastD'] + macd[,'macd'] + stochOSC[,'slowD'],
    family = "binomial"
  )
  AIC_values <- c(AIC_values, model$aic)
}

# RSI, n = 10
AIC_values <- numeric(0)
stochOSC <- stoch(btc_df[l1 ,c("High","Low","Close")], nFastK = 12, nFastD = 2, nSlowD = 5)
for(i in 10:20) {
  rsi <- RSI(btc_df[l1,"Close"], n = i)
  model <- glm(
    btc_df[l1,]$t_point ~ stochOSC[,'fastK'] + rsi + stochOSC[,'fastD'] + macd[,'macd'] + stochOSC[,'slowD'],
    family = "binomial"
  )
  AIC_values <- c(AIC_values, model$aic)
}