# Predicting Cryptocurrency rally using Logistic Regression

# Introduction
I will be using logistic regression to predict a arally in cryptocurrencies. I define a rally as successive second order higher highs and higher lows. First order higher highs are closing prices that are higher than both immediately preceding and succeeding closing prices. Second order higher highs are first order higher highs that are higher than preceding and succeeding first order higher highs. Likewise, the second order lower lows are first order lower lows that are lower than preceding and succeeding first order lower lows. 

![image](https://user-images.githubusercontent.com/79707074/109423661-70746200-7a06-11eb-8639-649b3588c58a.png)

In a typical chart highs and lows follow each other. Simiarly, second order higher highs are usually though not necessarily followed by second order lower lows. There is a possibility of market consolidating into a traingle/wedge pattern where the highs and lows of same order might not alternate.

![image](https://user-images.githubusercontent.com/79707074/109423846-217afc80-7a07-11eb-98b8-288809fcd0bb.png)
