# Predicting rally in prices of Bitcoin using Logistic Regression

# Introduction
Bitcoin has had a spectacular rally since September 2020 generating a 255% return since then. The rally has however been quite volatile with 11 separate sessions seeing intraday losses of 10% or more. While the long term returns on Bitcoin are subject to fundamental analysis with many believing that Bitcoin has topped out, there are still opportunities on smaller time frames that traders can exploit for profit. The aim of this research is to use and compare linear models to predict rally in Bitcoin prices on an hourly scale.

## Defining a turning point
Predicting a rally in Bitcoin involves predciting the truning pont of a sell-off. For this defining a rally is necessary. Here a rally is defined as a successive second order higher highs and higher lows. First order higher highs are closing prices that are higher than both immediately preceding and succeeding closing prices. Second order higher highs are first order higher highs that are higher than preceding and succeeding first order higher highs. Likewise, the second order lower lows are first order lower lows that are lower than preceding and succeeding first order lower lows. 

<img src="https://user-images.githubusercontent.com/79707074/109423661-70746200-7a06-11eb-8639-649b3588c58a.png" width="300">

In a typical chart highs and lows follow each other. Simiarly, second order higher highs are usually though not necessarily followed by second order lower lows. There is a possibility of market consolidating into a traingle/wedge pattern where the highs and lows of same order might not alternate.

<img src="https://user-images.githubusercontent.com/79707074/109423846-217afc80-7a07-11eb-98b8-288809fcd0bb.png" width="300">

# Data and Methodology

## Identifying useful regressors
![image](https://user-images.githubusercontent.com/79707074/126894325-04308c95-42a3-4157-a55c-ffb46d504028.png)

# Model
glm(formula = btc_df[l1, ]$t_point ~ stochOSC[, "fastK"] + rsi + 
    stochOSC[, "fastD"] + macd[, "macd"] + stochOSC[, "slowD"], 
    family = "binomial")

# Conclusion

