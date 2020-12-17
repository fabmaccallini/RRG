## Volatility indicators

# Average True Range (ATR)  [TTR]
# Class: Index
# Description: The ATR is a Welles Wilder style moving average of the True Range and is a measure of volatility.
#              Developed by J. Welles Wilder and published in his 1978 book 'New Concepts In Technical Trading Systems'.

ATR(HLC, n = 14, maType, ...)


# Bollinger Bands (BBands)  [TTR]
# Class: Overlay
# Description: Bollinger Bands are a volatility envelope around the price levels over a period of time.
#              Developed by John Bollinger.

BBands(HLC, n = 20, maType, sd = 2, ...)


# Chaikin Volatility (chaikinVolatility)  [TTR]
# Class: Oscillator
# Description: The Chaikin Volatility indicator is the rate of change of the trading range. The indicator defines volatility as an increasing 
#              of the difference between the high and low.
#              Developed by Marc Chaikin. 

chaikinVolatility(HL, n = 10, maType, ...)


# Inertia was presented by Donald Dorsey in september 1995 issue of S&C magazine.
# Inertia (RVI of High + RVI of Low ) /2
# Positive Inertia
# Inertia> 50
# Negative Inertia
# Inertia <50


# Relative Volatility Index (RVI, RVIr)
# Class: Oscillator
# Description: The Relative Volatility Index (RVI) is a volatility indicator based on RSI on a 9 period standard deviation of the price. 
#              Developed by Donald Dorsey and published in his article in the June, 1993 issue of Technical Analysis of Stocks & Commodities magazine,
#              and later revised (RVIr) in his article in the September, 1995 issue of the same magazine. 

RVI <- function (x, n = 10, maType = 'EMA', volType = 'runSD', ...) 
{
  x <- try.xts(x, error = as.matrix)
  vol <- get(maType, mode = "function")
  x.vol <- vol(x, n)
  up <- momentum(price, n = 1, na.pad = TRUE)
  dn <- up * 0
  which.dn <- which(up < 0)
  dn[which.dn] <- x.vol[which.dn]
  up[which.dn] <- 0
  which.up <- which(up > 0)
  up[which.up] <- x.vol[which.up]
  maArgs <- list(n = n, ...)
  if (missing(maType)) {
    maType <- "EMA"
    if (is.null(maArgs$wilder)) {
      maArgs$wilder <- TRUE
    }
  }
  if (is.list(maType)) {
    maTypeInfo <- sapply(maType, is.list)
    if (!(all(maTypeInfo) && length(maTypeInfo) == 2)) {
      stop ("If 'maType' is a list, you must specify\n ", 
           "*two* MAs (see Examples section of ?RVI)")
    }
    for (i in 1:length(maType)) {
      if (!is.null(formals(maType[[i]])$n) && is.null(maType[[i]]$n)) {
        maType[[i]]$n <- n
      }
      mavgUp <- do.call(maType[[1]][[1]], c(list(up), maType[[1]][-1]))
      mavgDn <- do.call(maType[[2]][[1]], c(list(dn), maType[[2]][-1]))
    }
  }
  else {
    mavgUp <- do.call(maType, c(list(up), maArgs))
    mavgDn <- do.call(maType, c(list(dn), maArgs))
  }
  rvi <- 100 * mavgUp/(mavgUp + mavgDn)
  if (!is.null(dim(rvi)) && ncol(rvi) == 1L) {
    colnames(rvi) <- "rvi"
  }
  reclass(rvi, price)
}

RVIr <- function (HL, n = 10, maType = 'EMA', volType = 'runSD', ...)
{
  rvi.h <- RVI(HL$High, n, matype, volType, ...)
  rvi.l <- RVI(HL$Low, n, matype, volType, ...)
  rvir <- (rvi.h + rvi.l) / 2
  return (rvir)
}


# Vertical Horizontal Filter (VHF)
# Class: Oscillator
# Description: The Vertical Horizontal Filter (VHF) determines the strength of a trend. When the VHF is rising, it indicates the formation of 
#              a trend. When the VHF is falling, it indicates the trend is ending and price is becoming congested
#              Developed by Adam White.

VHF(HLC, n = 28)

# https://www.dittotrade.academy/education/intermediate/technical-analysis/indicators/trending/brief-history-of-bands-and-envelopes/