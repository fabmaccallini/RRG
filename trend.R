## Trend indicators

# Absolute Price Oscillator (APO)
# Class: Oscillator
# Description: APO displays the difference between two exponential moving averages of a security's price and is expressed as an absolute value.

# Returned as output by the MACD function when percent = FALSE.


# Aroon Indicator  [TTR]
# Class: Oscillator
# Description: 'Aroon' is Sanskrit for "dawn's early light." The Aroon indicator consists of two lines (Up and Down) that measure how 
#              long it has been since the highest high/lowest low has occurred within an n period range.
#              When the Aroon is between 70 and 100 then it indicates a trend (Up or Down), strong if the other indicator reads below 30.
#              Developed by Tushar S. Chande and published in the September 1995 issue of Technical Analysis of Stocks & Commodities magazine.

aroon(HL, n = 20)


# Average Direction Movement Index (ADX)  [TTR]
# Class: Index
# Description: ADX is moving average of the Directional Movement Index (DX). The values range from 0 to 100, but rarely get above 60.
#              Developed by J. Welles Wilder and published in his 1978 book 'New Concepts In Technical Trading Systems'. 
# Signal: Bulllish if +DMI > -DMI and ADX crosses above 25
#         Bearish if -DMI > +DMI and ADX crosses above 25
#         Range bound when ADX is under 25

ADX(HLC, n = 14, maType, ...)


# Detrended Price Oscillator (DPO)  [TTR]
# Class: Oscillator
# Description: DPO removes the trend in prices by subtracting a 'n' moving average of the price from the price shifted n / 2 + 1 periods (lag).

DPO(x, n = 10, maType, shift = n / 2 + 1, percent = FALSE, ...)


# Directional Movement Index (DX)  [TTR]
# Class: Index
# Description: ADX is moving average of the Directional Movement Index (DX). The values range from 0 to 100, but rarely get above 60.
#              Developed by J. Welles Wilder and published in his 1978 book 'New Concepts In Technical Trading Systems'. 

# Returned as output by the ADX function


# Guppy Multiple Moving Averages (GMMA)  [TTR]
# Class: Index
# Description: The Guppy Multiple Moving Average signals a changing trend when the 'short' and 'long' groups of moving averages intersect.
#              An up/down trend exists when the short/long-term moving averages are greater than the long/short-term averages.
#              Developed by Daryl Guppy.

GMMA(x, short = c(3, 5, 8, 10, 12, 15), long = c(30, 35, 40, 45, 50, 60), maType)


# Keltner Bands
# Class: Overlay
# Description: Keltner Bands are volatility-based envelopes set above and below a moving average.
#              Developed by Chester Keltner and published in his 1960 book 'How to Make Money in Commodities'.
#              Linda Bradford Raschke introduced a newer version in the 1980s, using ATR instead HL for the bands.

KeltnerBands <- function (hlc, n_ma = 10, n_bands = 14, ma = 'SMA', bands = 'ATR', mult = 2, typical = TRUE) {
  if (typical) price <- apply(HLC(hlc), 1, sum) / 3 else price <- hlc$Close
  ma <- get(ma, mode = "function")
  mid <- ma(price, n_ma)
  if (bands == 'ATR') band <- ATR(HLC(hlc), n_bands)[, 2] else band <- ma(hlc$High - hlc$Low, n_bands)
  up <- mid + mult * band
  dn <- mid - mult * band
  kb <- cbind(dn, mid, up)
  colnames(kb) <- c("Kelt_dn", "Kelt_mid", "Kelt_up")
  return (kb)
}


# Moving Average Convergence Divergence (MACD)  [TTR]
# Class: Oscillator
# Description: MACD is the difference between two Exponential Moving Averages. The Signal line is an Exponential Moving Average of the MACD.
#              Developed by Gerald Appel.

MACD(x, nFast = 12, nSlow = 26, nSig = 9, maType, percent = TRUE, ...)


# Parabolic Stop And Reverse (SAR)
# Class: Overlay
# Description: The Parabolic SAR calculates a trailing stop and reverse point.
#              Developed by J. Welles Wilder and published in his 1978 book 'New Concepts In Technical Trading Systems'. 

SAR(HL, accel = c(0.02, 0.2))


# Percentage Price Oscillator (PPO)
# Class: Oscillator 
# Description:

PPO <- function (x, nFast = 12, nSlow = 26, maType = 'EMA', ...) {
  ma <- get(maType, mode = "function")
  ppo <- (ma(x, nFast) - ma(x, nSlow)) / ma(x, nSlow) * 100
  return (ppo)
}


# Trend Detection Index (TDI)  [TTR]
# Class: Overlay
# Description: The Trend Detection Index (TDI) attempts to identify starting and ending trends. A positive/negative TDI indicates a trend/consolidation.
#              The direction indicator determines the direction of the trend.
#              Developed by M. H. Pee.

TDI(price, n = 20, multiple = 2)


# Vertical Horizontal Filter (VHF)
# Class: Oscillator
# Description: The Vertical Horizontal Filter (VHF) determines the strength of a trend. When the VHF is rising, it indicates the formation of 
#              a trend. When the VHF is falling, it indicates the trend is ending and price is becoming congested
#              Developed by Adam White.

VHF(HLC, n = 28)