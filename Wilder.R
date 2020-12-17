function [adx, pdi, mdi, adxr] <- adx(ohlc, n = 14)

# Name:       Average Directional Movement Index.
# Author:     J. Welles Wilder, Jr.
# References: New Concepts in Technical Trading Systems  Trend Research, 1978, p. 36.
# Type:       Momentum indicator.
# Timing:     End.
# Value:      Measures the strength of the trend.
# 
# ADX (Average Directional Movement Index) is the average of the absolute
# value of the difference between PDI and MDI.
# PDI (Positive Directional Indicator or +DI) is the average percentage of 
# positive directional movement in the true range over N periods.
# MDI (Negative Directional Indicator or -DI) is the average percentage of 
# negative directional movement in the true range over N periods.
#
# INPUTS:
# H - high prices
# L - low prices
# C - close prices
# n - number of days of averaging


pdm1 <- [0 max(0, ohlc$High(2: end) - ohlc$High(1: end - 1))]
mdm1 <- [0 max(0, ohlc$Low(1: end - 1) - ohlc$Low(2: end))]
atr <- tsmovavg(truerange(H, L, C), 's', n, 1)
pdi <- tsmovavg(pdm1, 's', n, 1)./ atr * 100
mdi <- tsmovavg(mdm1, 's', n, 1)./ atr * 100
truedm <- pdi - mdi
adx <- tsmovavg(abs(truedm), 's', n, 1)
adxr <- (adx + [zeros(n, 1) adx(1: end - n)]) / 2


atr <- function (ohlc, n, average = "EWMA")

# Name:       Average True Range (ATR).
# Author:     J. Welles Wilder, Jr.
# References: New Concepts in Technical Trading Systems  Trend Research,
#             1978, p. 23.
# Type:       Volatility indicator.
# Timing:     Intra.
# Value:      Empirical estimate of volatility.
# Notes:      ATR is part of what Wilder called the Volatility System. ATR
#             is multiplied by a constant (ranging from 2.8 to 3.1) to
#             indentify a stop and reversal (SAR) level from the highest
#             (lowest) close.
#
# INPUTS:
# H - high prices
# L - low prices
# C - close prices
# n - number of days of averaging (7 by default)

tr <- true.range(ohlc)
atr <- ewma(tr, n)
end


function [adx, pdi, mdi] <- directionalmovemenindicator(H, L, C, n)

# Name:       Directional Indicator.
# Author:     J. Welles Wilder.
# References: New Concepts in Technical Trading Systems  Trend Research,
#             1978, p. 36.
# Type:       Momentum indicator.
# Timing:     End.
# Value:      Measures the strength of the trend.
# 


# +DM 
#
# INPUTS:
# H - high prices
# L - low prices
# C - close prices
# n - number of days of averaging

if ~exist('n', 'var')
n <- 7
end

pdm1 <- max(0, ohlc$High(2: end) - ohlc$High(1: end - 1))
mdm1 <- max(0, ohlc$Low(1: end - 1) - ohlc$Low(2: end))

function [adx, pdi, mdi] <- adx(H, L, C, n)

# Name:       Average Directional Movement Index.
# Author:     J. Welles Wilder.
# References: New Concepts in Technical Trading Systems  Trend Research,
#             1978, p. 36.
# Type:       Momentum indicator.
# Timing:     End.
# Value:      Measures the strength of the trend.
# 
# ADX (Average Directional Movement Index) is the average of the absolute
# value of the difference between PDI and MDI.
# PDI (Positive Directional Indicator or +DI) is the average percentage of 
# positive directional movement in the true range over N periods.
# MDI (Negative Directional Indicator or -DI) is the average percentage of 
# negative directional movement in the true range over N periods.
#
# INPUTS:
# H - high prices
# L - low prices
# C - close prices
# n - number of days of averaging

if ~exist('n', 'var')
n <- 7
end

pdm1 <- max(0, ohlc$High(2: end) - ohlc$High(1: end - 1))
mdm1 <- max(0, ohlc$Low(1: end - 1) - ohlc$Low(2: end))
atr <- averagetruerange(H, L, C, n)
pdi <- ewma(pdm1)./ atr
mdi <- ewma(mdm1)./ atr
truedm <- pdi - mdi
tsmovavg(tsobj,'s',lag,dim)



function r <- rsi(ohlc, n)

# Name:       Relative Strength Index (RSI).
# References: J. Welles Wilder, Jr. - New Concepts in Technical Trading Systems, Trend Research, 1978, p. 65.
# Type:       Momentum Oscillator.
# Timing:     Intra.
# Value:      Trend exaustion range.
# Note:       Overbought or oversold signals are given when RSI is above 70
#             or below 30.
#
# INPUTS:
# C - close prices
# n - number of days of averaging

# up and down moves
dx <- c(0, diff(ohlc$Close))
up <- dx * dx <= 0
dn <- abs(dx) * dx < 0

# calculate exponential moving averages
avg.up <- ewma(up, n)
avg.dn <- ewma(down, n)
r <- 100 * avg.up / (avg.up + avg.dn)
r[(up + down) == 0] <- 50

## NOTES: Wilder uses  exponentially smoothed simple moving average with ? <- 1/n.




true.range <- function (ohlc) {
  
  # References: New Concepts in Technical Trading Systems, Trend Research, 1978, p. 21.
  # Type:       Daily volatility indicator.
  # Timing:     Intra.
  # Value:      Empirical estimate of volatility.
  
  end <- dim(ohlc)[1]
  tr <- matrix(0, end, 3)
  tr[, 1] <-  ohlc$High - ohlc$Low
  tr[2: end, 2] <- ohlc$High[2: end] - ohlc$Close[1: (end - 1)]
  tr[2: end, 3] <- ohlc$Low[2: end] - ohlc$Close[1: (end - 1)]
  tr <- apply(tr, 1, max)
  
  return (tr)
}

ema <- function (r) {
  s <- 0
  extra <- 1
  list(
    update <- function (x) {
      s <- r * s + (1 - r) * x
      extra <- r * extra
      s / (1 - extra)
    }
  )
}

x <- c(1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
