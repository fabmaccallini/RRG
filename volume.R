## Volume Indicators

# Accumulation Distribution Line (chaikinAD)  [TTR]
# Class: Cumulative
# Description: Accumulation Distribution Line measures the proximity of closing prices to their highs or lows weighted by 
#              volume to determine if accumulation or distribution is occurring in the market.
#              It was developed by Marc Chaikin.

chaikinAD(HLC, volume)


# Arms Index (TRIN)
# Class: Oscillator
# Description: The Arms Index shows whether volume is moving into advancing or declining issues. Values below/above 1.0 
#              indicate more volume in advancing/declining issues. Values over 1.2 indicate oversold conditions, values under 
#              0.7 indicate overbought.
#              It was developed by Richard W. Arms, Jr. in 1967.

TRIN <- function (price, volume, n = 20) {
  p <- as.numeric(price)
  v <- as.numeric(volume)
  p.up <- tail(p, -1) > head(p, -1)
  p.dn <- tail(p, -1) < head(p, -1)
  p.ratio <- runSum(p.up, n) / runSum(p.dn, n)
  v.up <- tail(v, -1) > head(v, -1)
  v.dn <- tail(v, -1) < head(v, -1)
  v.ratio <- runSum(v.up, n) / runSum(v.dn, n)
  trin <- c(NA, p.ratio / v.ratio)
  reclass(trin, price)
}


# Chaikin Money Flow (CMF)  [TTR]
# Class: Cumulative
# Description: Chaikin Money Flow (CMF) developed by Marc Chaikin is a volume-weighted average of accumulation and 
#              distribution over a specified period. If the price action consistently closes above/below the bar's midpoint 
#              on increasing volume, the Chaikin Money Flow will be positive/negative value.
#              It was developed by Marc Chaikin..

CMF(HLC, volume, n = 20)


# On Balance Volume (OBV)  [TTR]
# Class: Cumulative
# Description: On Balance Volume (OBV) measures buying and selling pressure as a cumulative indicator that adds volume on up 
#              days and subtracts volume on down days.  
#              Developed by Joseph Granville and published in his 1963 book, 'New Strategy of Daily Stock Market Timing for Maximum Profit'.

OBV(price, volume)


# Positive/Negative Volume Index (PVI)
# Class: Cumulative
# Description: PVI assumes that on days when volume increases, crowd-following "uninformed" investors are entering the market. Conversely, on days 
#              with decreased volume, the "smart money" is quietly taking positions.  
#              Developed eveloped by Paul Dysart in the 1930s, republished by by Norman Fosback in his 1976 book 'Stock Market Logic'.

PVI <- function (price, volume) {
  p <- as.numeric(price)
  v <- as.numeric(volume)
  vol.up <- tail(v, -1) > head(v, -1)
  ret <- log(tail(p, -1) / head(p, -1))
  pvi <- (1 + ret) * vol.up
  pvi[pvi == 0] <- 1
  pvi <- c(NA, cumprod(pvi))
  reclass(pvi, price)
}

NVI <- function (price, volume) {
  p <- as.numeric(price)
  v <- as.numeric(volume)
  vol.dn <- tail(v, -1) < head(v, -1)
  ret <- log(tail(p, -1) / head(p, -1))
  nvi <- (1 + ret) * vol.dn
  nvi[nvi == 0] <- 1
  nvi <- c(NA, cumprod(nvi))
  reclass(nvi, price)
}

# Volume Oscillator (VO)
# Class: Oscillator
# Description: The Volume Oscillator (VO) is the difference between two moving averages of a security's volume expressed as a percentage.

VO <- PPO <- function (x, nFast = 12, nSlow = 26, maType = 'EMA', ...) {
  maArgs <- list(...)
  ma <- get(maType, mode = "function")
  ppo <- (ma(x, nFast, maArgs) - ma(x, nSlow, maArgs)) / ma(x, nSlow, maArgs) * 100
  return (ppo)
}