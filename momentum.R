## Momentum Indicators

# Chande Momentum Oscillator (CMO)  [TTR]
# Class: Oscillator
# Description: The Chande Momentum Oscillator (CMO) is a modified RSI.
#              Developed by Tushar S. Chande.

CMO(x, n = 14)


# Commodity Channel Index  [TTR]
# Class: Oscillator
# Description:

CCI(HLC, n = 20, maType, c = 0.015, ...)


# Double Smoothed Stochastic  [TTR]
# Class: Oscillator
# Description: The Double Smoothed Stochastic indicator applies Exponential Moving Averages of two 
#              different periods to a standard Stochastic %K.
#              Developed by William Blau.

# stoch() function with smooth = 2


# DV Intermediate oscillator (DVI)  [TTR]
# Class: Oscillator
# Description: The DV Intermediate oscillator (DVI) is a very smooth momentum oscillator that can also be used as a trend indicator.
#              Created by David Varadi.

DVI(price, n = 252, wts = c(0.8, 0.2), smooth = 3, magnitude = c(5, 100, 5), stretch = c(10, 100, 2), exact.multiplier = 1)


# Know Sure Thing (KST)  [TTR]
# Class: Oscillator
# Description: The Know Sure Thing (KST) is a smooth, summed, rate of change indicator.
#              Developed by Martin Pring.

KST(price, n = c(10, 10, 10, 15), nROC = c(10, 15, 20, 30), nSig = 9, maType, wts = 1: NROW(n), ...)


# Momentum  [TTR]
# Class: Oscillator
# Description: The Momentum Oscillator measures the amount that a security’s price has changed over a given period of time.

momentum(x, n = 1, na.pad = TRUE)


# Money Flow Indicator (MFI)  [TTR]
# Class: Oscillator
# Description: The Money Flow Index (MFI) is a momentum indicator that measures the flow of money into and out of a security 
#              over a specified period of time. It is related to the Relative Strength Index (RSI) but incorporates volume.

MFI(HLC, volume, n = 14)


# Rate of Change (ROC)  [TTR]
# Class: Oscillator
# Description: Rate of Change (ROC) is a relative momentum indicator where the absolute price performance over n periods is normalised by the price.

ROC(x, n = 1, type = c("continuous", "discrete"), na.pad = TRUE)


# Realtive Strength Index  [TTR]
# Class: Oscillator
# Description: The Relative Strength Index (RSI) measures the speed and change of price movements. It oscillates between zero and 100, generally 
#              considered overbought when above 70 and oversold when below 30. 

RSI(price, n = 14, maType, ...)


# Relative Vigor Index
# Class: Oscillator
# Description: The Relative Vigor Index (RVI) is an oscillator based on the concept that prices tend to close higher than they
#              open in up trends and close lower than they open in down trends.

RVI(OHLC, n = 20, maType = 'SMA', ...) {
  o <- as.numeric(OHLC$Open)
  h <- as.numeric(OHLC$High)
  l <- as.numeric(OHLC$Low)
  c <- as.numeric(OHLC$Close)
  
  co <- c - o
  hl <- h - l
  nm <- c(rep(NA, 3), tail(co, -3) + 2 * tail(head(co, -1), -2) + 2 * tail(head(co, -2), -1) + head(co, -3)) / 6
  dn <- c(rep(NA, 3), tail(hl, -3) + 2 * tail(head(hl, -1), -2) + 2 * tail(head(hl, -2), -1) + head(hl, -3)) / 6
  ma <- get(maType, mode = "function")
  rvi <- ma(nm, n) / ma(dn, n)
  sig <- c(rep(NA, 3), tail(rvi, -3) + 2 * tail(head(rvi, -1), -2) + 2 * tail(head(rvi, -2), -1) + head(rvi, -3)) / 6
  rvi <- data.frame(rvi = rvi, sig = sig)
  reclass(rvi, OHLC)
}


# Signal to Noise Ratio (SNR)  [TTR]
# Class: Oscillator
# Description: Signal to Noise Ratio (SNR) is a relative momentum indicator where the absolute price performance over n periods is normalised by its ATR.
#              Skeggs, James and Hill, Alex (2015). Back in Black Part 2: The Opportunity Set for Trend Following.

SNR(HLC, n, ...)


# Stochastic (stoch)  [TTR]
# Class: Oscillator
# Description: The stochastic oscillator is a momentum indicator that relates the location of each day's close relative to the high/low 
#              range over the past n periods.
#              Developed by George C. Lane in the late 1950s.

stoch(HLC, nFastK = 14, nFastD = 3, nSlowD = 3, maType, bounded = TRUE, smooth = 1, ...)


# Stochastic Momentum Index (SMI)  [TTR]
# Class: Oscillator
# Description: The SMI is a momentum indicator that relates the close to the midpoint of the high/low range.
#              Developed by William Blau in 1993.

SMI(HLC, n = 13, nFast = 2, nSlow = 25, nSig = 9, maType, bounded = TRUE, ...)


# StochRSI (stochRSI)  [TTR]
# Class: Oscillator
# Description: The Stochastic RSI indicator, developed by Tushard Chande and Stanley Kroll, is an oscillator that uses RSI values, instead of price values, as inputs in the Stochastic formula. The indicator measures where the RSI’s current value is relative to its high/low range for the specified period.

stochRSI(x, n = 14L)


# Triple Smoothed Exponential Oscillator (TRIX)  [TTR]
# Class: Oscillator
# Description: The TRIX indicator calculates the rate of change of a triple exponential moving average.
#              Developed by Jack K. Hutson.

TRIX(price, n = 20, nSig = 9, maType, percent = TRUE, ...)


# Ultimate Oscillator (ultimateOscillator)  [TTR]
# Class: Oscillator
# Description: The Ultimate Oscillator is a momentum oscillator designed to capture momentum across three different time frames.
#              Developed by Larry Williams in 1976.

ultimateOscillator(HLC, n = c(7, 14, 28), wts = c(4, 2, 1))


# Williams Accumulation / Distribution (williamsAD)  [TTR]
# Class: Index
# Description: The Williams Accumulation / Distribution line is a measure of market momentum.
#              Developed by Larry Williams.

williamsAD(HLC)


# Williams %R  [TTR]
# Class: Oscillator
# Description: The Williams Accumulation / Distribution line is a measure of market momentum.
#              Developed by Larry Williams. Similar to stochastics' fast %K.

WPR(HLC, n = 14)