TD.camouflage <- function(ohlc, opt = c(1, 2), var = TRUE) {
  
  # References: Jason Perl, DeMark Indicators; Bloomberg 2008, page 25, 37.
  # Type:       Breakout indicator
  
  # Arguments:
  # ohlc - historical OHLC price time serie; as dataframe (with a date field) or xts/zoo object
  # opt  - [1] number of days to look back for the Open/Close (1 by default), [2] number of days to look back for the High/Low (2 by default); as numeric
  # var  - varinant taking into account the True Low/High; as logical
  #
  # Value:
  # returns dataframe 
  # res  - 1 for Buy signals, -1 Sell signals or 0 otherwise; as numeric
  
  truehl <- true.hl(ohlc)
  end <- dim(ohlc)[1]
  cond1.b <- cond2.b <- cond3.b <- cond1.s <- cond2.s <- cond3.s <- res <- rep(0, end)
  
  # Buy
  # C(t) < C(t-1)
  cond1.b[(1 + opt[1]): end] <- ohlc$Close[(1 + opt[1]): end] < ohlc$Close[1: (end - opt[1])]
  # C(t) < O(t)
  cond2.b[(1 + opt[1]): end] <- ohlc$Close[(1 + opt[1]): end] > ohlc$Open[(1 + opt[1]): end]
  if (var) {
    # L(t) < TL(t-2)
    cond3.b[(1 + opt[2]): end] <- ohlc$Low[(1 + opt[2]): end] < truehl[1: (end - opt[2]), 2]
  } else {
    # L(t) < L(t-2)
    cond3.b[(1 + opt[2]): end] <- ohlc$Low[(1 + opt[2]): end] < ohlc$Low[1: (end - opt[2])]   
  } 

  # Sell
  # C(t) > C(t-1)
  cond1.s[(1 + opt[1]): end] <- ohlc$Close[(1 + opt[1]): end] > ohlc$Close[1: (end - opt[1])]
  # C(t) > O(t)
  cond2.s[(1 + opt[1]): end] <- ohlc$Close[(1 + opt[1]): end] < ohlc$Open[(1 + opt[1]): end]
  if (var) {
    # H(t) < TH(t-2)
    cond3.b[(1 + opt[2]): end] <- ohlc$High[(1 + opt[2]): end] < truehl[1: (end - opt[2]), 1]
  } else {
    # H(t) < H(t-2)
    cond3.s[(1 + opt[2]): end] <- ohlc$High[(1 + opt[2]): end] > ohlc$High[1: (end - opt[2])]
  }
  
  res <- cond1.b * cond2.b * cond3.b - cond1.s * cond2.s * cond3.s
  
  return (res)
}


TD.clop <- function (ohlc, opt = c(1)) {
  
  # Reference: Tom R. DeMark, New Market Timing Techniques Wiley 1997, page 240.
  #            Jason Perl, DeMark Indicators; Bloomberg 2008, pp. 25, 38.
  #
  # Arguments:
  # ohlc - historical OHLC price time serie; as dataframe (with a date field) or xts/zoo object
  # opt  - [1] number of days to look back for the Open/Close (1 by default); as numeric
  #
  # Value:
  # returns dataframe 
  # res  - 1 for Buy signals, -1 Sell signals or 0 otherwise; as numeric
  
  end <- dim(ohlc)[1]
  cond1.b <- cond2.b <- cond3.b <- cond4.b <- cond1.s <- cond2.s <- cond3.s <- cond4.s <- res <- rep(0, end)
  
  # Buy
  # O(t) < min(C(t-1), O(t-1))
  cond1.b[(1 + opt[1]): end] <- ohlc$Open[(1 + opt[1]): end] < ohlc$Close[1: (end - opt[1])]
  cond2.b[(1 + opt[1]): end] <- ohlc$Open[(1 + opt[1]): end] < ohlc$Open[1: (end - opt[1])]
  # P(t) > max(C(t-1), O(t-1))
  cond3.b[(1 + opt[1]): end] <- ohlc$High[(1 + opt[1]): end] > ohlc$Close[1: (end - opt[1])]
  cond4.b[(1 + opt[1]): end] <- ohlc$High[(1 + opt[1]): end] > ohlc$Open[1: (end - opt[1])]
  # Sell
  # O(t) > min(C(t-1), O(t-1))
  cond1.s[(1 + opt[1]): end] <- ohlc$Open[(1 + opt[1]): end] > ohlc$Close[1: (end - opt[1])]
  cond2.s[(1 + opt[1]): end] <- ohlc$Open[(1 + opt[1]): end] > ohlc$Open[1: (end - opt[1])]
  # P(t) < max(C(t-1), O(t-1))
  cond3.s[(1 + opt[1]): end] <- ohlc$Low[(1 + opt[1]): end] < ohlc$Close[1: (end - opt[1])]
  cond4.s[(1 + opt[1]): end] <- ohlc$Low[(1 + opt[1]): end] < ohlc$Open[1: (end - opt[1])]
  
  res <- cond1.b * cond2.b * cond3.b * cond4.b - cond1.s * cond2.s * cond3.s * cond4.s
  
  return (res)
}


TD.clopwin <- function (ohlc, opt = c(1)) {
  
  # Reference: Tom R. DeMark, New Market Timing Techniques Wiley 1997, page 240.
  #            Jason Perl, DeMark Indicators; Bloomberg 2008, pp. 26, 38.
  #
  # Arguments:
  # ohlc - historical OHLC price time serie; as dataframe (with a date field) or xts/zoo object
  # opt  - [1] number of days to look back for the High/Low/Close (1 by default); as numeric
  #
  # Value:
  # returns dataframe 
  # res  - 1 for Buy signals, -1 Sell signals or 0 otherwise; as numeric
  
  end <- dim(ohlc)[1]
  cond1 <- cond2 <- cond3 <- cond4 <- cond5.b <- cond5.s <- res <- rep(0, end)
  
  # O(t) & C(t) within L(t-1) & H(t-1)
  cond1[(1 + opt[1]): end] <- ohlc$Open[(1 + opt[1]): end] < ohlc$High[1: (end - opt[1])]
  cond2[(1 + opt[1]): end] <- ohlc$Open[(1 + opt[1]): end] > ohlc$Low[1: (end - opt[1])]
  cond3[(1 + opt[1]): end] <- ohlc$Close[(1 + opt[1]): end] < ohlc$High[1: (end - opt[1])]
  cond4[(1 + opt[1]): end] <- ohlc$Close[(1 + opt[1]): end] > ohlc$Low[1: (end - opt[1])]
  
  # Buy
  # c(t) > C(t-1)
  cond5.b[(1 + opt[1]): end] <- ohlc$Close[(1 + opt[1]): end] > ohlc$Close[1: (end - opt[1])]
  
  # sell
  # c(t) < C(t-1)
  cond5.s[(1 + opt[1]): end] <- ohlc$Close[(1 + opt[1]): end] < ohlc$Close[1: (end - opt[1])]
  
  res <- cond1 * cond2 * cond3 * cond4 * (cond5.b - cond5.s)
  
  return (res)
}


TD.open <- function (ohlc, opt = c(1)) {
  
  # References: Tom R. DeMarkNew Market Timing Techniques Wiley 1997, page 238.
  #             Jason Perl, DeMark Indicators; Bloomberg 2008, pp. 26, 38.
  #
  # Arguments:
  # ohlc - historical OHLC price time serie; as dataframe (with a date field) or xts/zoo object
  # opt  - [1] number of days to look back for the High/Low (1 by default); as numeric
  #
  # Value:
  # returns dataframe 
  # res  - 1 for Buy signals, -1 Sell signals or 0 otherwise; as numeric
  
  end <- dim(ohlc)[1]
  cond1.b <- cond2.b <- cond1.s <- cond2.s <- res <- rep(0, end)
  
  # Buy
  # O(t) < L(t-1)
  cond1.b[(1 + opt[1]): end] <- ohlc$Open[(1 + opt[1]): end] < ohlc$Low[1: (end - opt[1])]
  # P(t) > L(t-1)
  cond2.b[(1 + opt[1]): end] <- ohlc$High[(1 + opt[1]): end] > ohlc$Low[1: (end - opt[1])]
  # Sell
  # O(t) > H(t-1)
  cond1.s[(1 + opt[1]): end] <- ohlc$Open[(1 + opt[1]): end] > ohlc$High[1: (end - opt[1])]
  # P(t) < H(t-1)
  cond2.s[(1 + opt[1]): end] <- ohlc$Low[(1 + opt[1]): end] < ohlc$High[1: (end - opt[1])]
  
  res <- cond1.b * cond2.b - cond1.s * cond2.s
  
  return (res)
}


TD.trap <- function (ohlc, opt = c(1, 1), var = FALSE) {
  
  # References: Tom R. DeMarkNew Market Timing Techniques Wiley 1997, page 238.
  #             Jason Perl, DeMark Indicators; Bloomberg 2008, pp. 26, 38.
  #
  # Arguments:
  # ohlc - historical OHLC price time serie; as dataframe (with a date field) or xts/zoo object
  # opt  - [1] number of days to look back for the Open/Close (1 by default), [2] additional filters: 0, no filters [by default] 1, filters on, [3] number of days to look back from opt[1] within the filter [1 by default].; as numeric
  #
  # Value:
  # returns dataframe 
  # res  - 1 for Buy signals, -1 Sell signals or 0 otherwise; as numeric

  end <- dim(ohlc)[1]
  cond1 <- cond2 <- cond3.b <- cond4.b <- cond5.b <- cond3.s <- cond4.s <- cond5.s <- res <- rep(0, end)
  
  # O(t) within L(t-1) & H(t-1)
  cond1[(1 + opt[1] + opt[2]): end] <- ohlc$Open[(1 + opt[1] + opt[2]): end] < ohlc$High[(1 + opt[2]): (end - opt[1])]
  cond2[(1 + opt[1] + opt[2]): end] <- ohlc$Open[(1 + opt[1] + opt[2]): end] > ohlc$Low[(1 + opt[2]): (end - opt[1])]
  # Buy
  # P(t) > H(t-1)
  cond3.b[(1 + opt[1] + opt[2]): end] <- ohlc$High[(1 + opt[1] + opt[2]): end] > ohlc$High[(1 + opt[2]): (end - opt[1])]
  # Sell
  # P(t) < L(t-1)
  cond3.s[(1 + opt[1] + opt[2]): end] <- ohlc$Low[(1 + opt[1] + opt[2]): end] > ohlc$Low[(1 + opt[2]): (end - opt[1])]

  # Additional filters @@@ from where?
  if (var) {
    # Buy
    cond4.b[(1 + opt[2]): end] <- ohlc$High[(1 + opt[2]): end] > ohlc$High[1: (end - opt[2])]
    cond5.b[(1 + opt[2]): end] <- ohlc$Close[(1 + opt[2]): end] > ohlc$Close[1: (end - opt[2])]
    # Sell
    cond4.s[(1 + opt[2]): end] <- ohlc$Low[(1 + opt[2]): end] < ohlc$Low[1: (end - opt[2])]
    cond5.s[(1 + opt[2]): end] <- ohlc$Close[(1 + opt[2]): end] < ohlc$Close[1: (end - opt[2])]
    
    res <- cond1 * cond2 * (cond3.b * cond4.b * cond5.b - cond3.s * cond4.s * cond5.s)
  } else {
    res <- cond1 * cond2 * (cond3.b - cond3.s)
  }
  
  return (res)
}