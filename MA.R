### Moving averages

# NOTES: Generalise some of the functions for any basic MA

# to source it
n = 10; volume = rep(1, dim(x)[1]); price = x; w <- seq(1, 0, -1 / (dim(x)[1] - 1))

library (TTR)

# Simple Moving Average  [TTR]
SMA(x, n = 10)

# Esponential Moving Average  [TTR]
# notes: uses an exponential smoothing ratio of 2/(n+1), while wilder=TRUE uses a ratio of 1/n
EMA(x, n = 10, wilder = FALSE, ratio = NULL)

# Weighted Moving Average  [TTR]
WMA(x, n = 10, wts = 1: n)

# Double Esponential Moving Average (generalised) [TTR]
# reference: Patrick Mulloy - "Smoothing Data with Faster Moving Averages", Technical Analysis of Stocks & Commodities magazine, February 1994.
#            Tim Tillson - "Better Moving Averages", Technical Analysis of Stocks & Commodities magazine, November 1998. (generalised version)
# notes: beta = 1 is a standard DEMA, while beta = 0 is an EMA
DEMA(x, n = 10, v = 1, wilder = FALSE, ratio = NULL)

# Triple Esponential Moving Average (generalised)
# reference: Patrick Mulloy - "Smoothing Data with Faster Moving Averages", Technical Analysis of Stocks & Commodities magazine, February 1994.
# notes: beta = 1 is a standard TEMA, while beta = 0 is a standard DEMA
TEMA <- function (x, n = 10, beta = 1, wilder = FALSE, ratio = NULL) 
{
  if (beta < 0 || beta > 1) {
    stop("Please ensure 0 <= beta <= 1")
  }
  tema <- (2 + beta) * (EMA(x, n, wilder, ratio) - EMA(EMA(x, n, wilder, ratio), n, wilder, ratio)) 
  + EMA(EMA(EMA(x, n, wilder, ratio), n, wilder, ratio), n, wilder, ratio) * beta
  if (!is.null(dim(tema))) {
    colnames(tema) <- "TEMA"
  }
  return(tema)
}

# T3 Moving Average
# reference: Tim Tillson - "Better Moving Averages", Technical Analysis of Stocks & Commodities magazine, November 1998.
T3 <- function (x, n = 10, beta = 1) DEMA(DEMA(DEMA(x, n, beta), n, beta), n, beta)

# Elastic Volume Weighted Moving Average  [TTR]
EVWMA(price, volume, n = 10)

# Zero Lag Esponential Moving Average  [TTR]
ZLEMA(x, n = 10, ratio = NULL)

# Volume Weighted Moving Average  [TTR]
VWAP(price, volume, n = 10)
VWMA(price, volume, n = 10)

# Variable-length Moving Average  [TTR]
VMA(x, w, ratio = 1)

# Hull Moving Average (generalised)
# reference: Alan Hull - http://alanhull.com/hull-moving-Average
# notes: TTR function not generalised with beta
HMA <- function (x, n = 20, beta = 1, ...)
{
  reclass(WMA((1 + beta) * WMA(x, n = n / 2, ...) - beta * WMA(x, n = n, ...), n = trunc(sqrt(n)), ...), x)
}

# Generic Hull Moving Average
GHMA <- function (x, n = 10, beta = 1, ma = "EMA", ...)
{
  ma <- get(ma, mode = "function")
  reclass(ma((1 + beta) * ma(x, n = n / 2, ...) - beta * ma(x, n = n, ...), n = trunc(sqrt(n)), ...), x)
}

# Arnaud Legoux Moving Average  [TTR]
ALMA(x, n = 9, offset = 0.85, sigma = 6)

# library (DSTrading)
# 
# # Fractal Adaptive Moving Average  [DSTrading]
# # reference: John Ehlers - Technical Analysis of Stocks & Commodities, October 2005.
# FRAMA(HLC, n = 20, FC = 1, SC = 200, triggerLag = 1, ...)
# 
# # Generalised Fractal Adaptive Moving Average  [DSTrading]
# # reference: John Ehlers - 
# GFRAMA(x, n = 20, FC = 1, SC = 200, ...)
# 
# # Kaufman Adaptive Moving Average  [DSTrading]
# # reference: Perry J. Kaufman - The New Commodity Trading Systems and Methods, New York: John Wiley & Sons, 1978, pp. 58-64
# KAMA(HLC, nER = 10, nFast = 2, nSlow = 30, priceMethod = "Close")
# 
# # Variable Index Dynamic Average  [DSTrading]
# # reference: Tushar Chande, Stanley Kroll - The New Technical Trader, John Wiley & Sons 1984.
# #            Tushar Chande - Technical Analysis of Stocks & Commodities, March 1992.
# VIDA(x, n = 9, m = 30, smoothConstant = .2, ...)

# # Ehler's Predictive Moving Averages  [DSTrading]
# # reference: John Ehlers - Rocket Science For Traders. , John Wiley & Sons 2001.
# PMA(x, n = 9, m = 30, smoothConstant = .2, ...)


# Nyquist Moving Average
NMA <- function (x, n = 10, lambda = 2, ma = "EMA")
{
  if (lambda < 2) {
    stop("Please ensure lambda >= 2")
  }
  if (n == lambda) {
    stop ("n must have a different value than lambda")
  }
  ma <- get(ma, mode = "function")
  alpha <- lambda * (n - 1) / (n - lambda)
  nma <- (1 + alpha) * ma(x, n) - alpha * ma(ma(x, n), n / lambda)
  colnames(nma) <- "NMA"
  return (nma)
}

# Triangular Moving Average
TMA <- function (x, n = 10) 
{
  tma <- SMA(SMA(x, n), n)
  if (!is.null(dim(tma))) {
    colnames(tma) <- "TMA"
  }
  return(tma)
}

# Endpoint Moving Average, Least Squares Moving Average (LSQMA), Moving Linear Regression, Time Series Forecast (TSF)
EPMA <- function (x, n = 10) {
  epma <- WMA(x, n, wts = seq(2 * n - 1, -n + 2, 3))
  return(epma)
}





# reference: John Ehlers - 
# Guppy Multiple Moving Average



#Laguerre Filter
#Median-Average Adaptive Filter
#Sine Weighted Moving Average
#Instantaneous Trendline by J.Ehlers
#Moving Median
#Geometric Mean
#Regularized EMA by Chris Satchwell
#Integral of Linear Regression Slope (ILRS) 
#Combination of LSMA and ILRS
#Triangular Moving Average generalized by J.Ehlers
#Smoothing by Mark Jurik
#Simplified SMA
#McGinley Dynamic




#Reduced Lag Moving Averages
#Zero Lag EMA (ZL-EMA)
#Almost Zero Lag EMA (AZL-EMA)
#Zero Lag Error Correcting EMA (EC-EMA)
#Modified Moving Average (M-MA)
#3rd Generation Moving Average (3G-MA)


# Volatility Measures
#Standard Deviation Ratio (SDR) - ratio between two standard deviations with different length
#Efficiency Ratio (ER)
#Relative Volatility Index (RVI)
#Vertical Horizontal Filter (VHF)
#Fractal Dimension (D)
#Z Score (ZS)
#Chaikin’s Volatility (CV) >
#Dreiss Choppiness Index (CI) >

#Variable Moving Average (VMA) aka Volatility Index Dynamic Average (VIDYA)
#Standard Deviation Ratio – Completed – Results (SDR-VMA)
#Efficiency Ratio – Completed – Results (ER-VMA)
#Relative Volatility Index – Completed – Results (RVI-VMA)
#Vertical Horizontal Filter – Completed – Results (VHF-VMA)
#Fractal Dimension Completed – Results (D-VMA) 
#Adaptive Moving Average (AMA) aka Kaufman Adaptive Moving Average (KAMA)
#Efficiency Ratio – Completed – Results (ER-AMA)
#Fractal Dimension – Completed – Results (D-AMA)
#Standard Deviation Ratio – Completed – Results (SDR-AMA)
#Relative Volatility Index – Completed – Results (RVI-AMA)
#Vertical Horizontal Filter – Completed – Results (VHF-AMA) 
#Log Normal Moving Average (LAMA)
#Fractal Adaptive Moving Average (FRAMA) – Completed – Results
#Standard Deviation Ratio
#Efficiency Ratio
#Relative Volatility Index
#Vertical Horizontal Filter

#Other Intelligent Moving Averages
#McGinley Dynamic Indicator
#MESA Adaptive Moving Average and Following Average FAMA (MAMA)
#- See more at: http://etfhq.com/blog/2010/05/25/best-technical-indicators/#sthash.jGwY6gL2.dpuf


getAnywhere(MACD)

