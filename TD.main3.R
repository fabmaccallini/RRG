TD <- list()

TD$Setup <- function (ohlc, opt = c(4, 9, 1), perf = TRUE, tdst = TRUE, risk = TRUE, date = FALSE, ...) {

  # Type:       Momentum indicator
  # Value:      Low-risk entry points within trending market
  #
  # Note: TD Setup can extend beyond the opt[2] count, in fact any new TD Setup can only start after a TD Price Flip (i.e. C[t-1] > C[t-5]
  #       followed by C[t] < C[t-4] and vice versa).
  #
  # Arguments:
  # ohlc - historical OHLC price time serie, as dataframe (with a date field) or xts/zoo object
  # opt  - number of days to look back (4 by default), consecutive number of counts for completion (9 by default) and number of points back
  #        from the first counting to calculate TDST, as numeric
  # perf - flag for perfection to be calculated
  # tdst - flag for TD Setup Trend price levels to be calculated
  # risk - flag for TD Risk levels to be calculated
  #
  # Value:
  # returns dataframe
  # cond1  - 1 if condition1 is met or 0 otherwise, as logical
  # count  - counts the sequence of cond1, positive for TD Buy Setup and negative for TD Sell Setup, as numeric
  # endsq  - marks with 1 the end of the streak TD Buy counts, with -1 TD Sell counts or 0 otherwise, as numeric
  # compl  - 1 for TD Buy Setup completion to opt[2], -1 for TD Sell Setup completion or 0 otherwise, as numeric
  # setup  - sequence from 1 to opt[2] for TD Buy Setup, -1 to -opt[2] for TD Sell Setup or 0 otherwise, as numeric
  # perf   - 1 if the perfection occurs or 0 otherwise, as logical
  # tdst   - TD Setup Trend price levels or 0 otherwise, as numeric
  # risk   - TD Risk levels or 0 otherwise, as numeric
  # @@@ NOTE: use different levels than true HL
  
  end <- dim(ohlc)[1]
  setup <- data.frame(matrix(0, dim(ohlc)[1], 5 + (perf + tdst + risk)))
  tmp <- c("perf", "tdst", "risk")
  colnames(setup) <- c("cond1", "count", "endsq", "compl", "setup", tmp[c(perf, tdst, risk)])
  
  ## TD Setup
  # References: Tom Demark, The New Science of Technical Analysis; Wiley 1994, p. 140
  #             ?New Market Timing Techniques; Wiley 1997, p. 43
  #             ?DeMark on day-trading options; McGrow-Hill 1999, pp. 111, 158
  #             Jason Perl, DeMark Indicators; Bloomberg 2008, pp. 3, 4
  
  # Condition 1
  tmp.b <- ohlc$Close[(opt[1] + 1): end] < ohlc$Close[1: (end - opt[1])]
  tmp.s <- ohlc$Close[(opt[1] + 1): end] > ohlc$Close[1: (end - opt[1])]
  setup$cond1[(opt[1] + 1): end] <- ifelse(length(tmp.b) > 0, tmp.b, 0) - ifelse(length(tmp.s) > 0, tmp.s, 0)
  
  # Counting sequence
  for (i in 2: (end - opt[1])) {
    if (tmp.b[i] == 1) tmp.b[i] <- tmp.b[i - 1] + 1
    if (tmp.s[i] == 1) tmp.s[i] <- tmp.s[i - 1] + 1
  }
  setup$count[(opt[1] + 1): end] <- tmp.b - tmp.s
  
  # End sequence
  tmp.b <- (head(tmp.b, -1) - tail(tmp.b, -1)) > 0
  tmp.s <- (head(tmp.s, -1) - tail(tmp.s, -1)) > 0
  setup$endsq[(opt[1] + 1): (end - 1)] <- tmp.b - tmp.s
  if (abs(setup$count[end]) > 0) setup$endsq[end] <- sign(setup$count[end]) # the last point counts as end of the sequence
  
  # Completion
  setup$compl <- (abs(setup$count) == opt[2]) * sign(setup$count)
  
  # Setup
  tmp.b <- which(setup$compl == 1)
  tmp.s <- which(setup$compl == -1)
  for (i in 0: (opt[2] - 1)) {
    setup$setup[tmp.b - i] <- opt[2] - i
    setup$setup[tmp.s - i] <- i - opt[2]
  }
  
  ## Perfection
  if (perf) setup$perf <- TD$Perfection(ohlc, opt, setup)

  ## Setup Trend Levels
  if (tdst) setup$tdst <- TD$ST(ohlc, opt, setup, levels = "true.hl")
  
  ## TD Risk Levels
  if (risk) setup$risk <- TD$Risk(ohlc, setup, opt, shift = "TR")

  if (date) rownames(setup) <- ohlc$Date
  
  return (setup)
}

## Trading
# required before taking a position: 1. perfection, 2. no TDST line crossed by any close, 3. C(9) in proximity of TDST line


TD$Perfection <- function (ohlc, opt, setup, lookback = c(2, 3)) {
  
  ## Perfection
  
  # Arguments:
  # perf - bars back from completion
  
  # References: 
  #             Jason Perl, DeMark Indicators; Bloomberg 2008, pp. 11, 14
  # Perl: at least one of the bars from 8 onward must verify that L(8+) <= L(7) & L(8+) <= L(6), vice versa H(8+) >= H(7) & H(8+) >= H(6)
  
  #%%% make perfection available for also for O and C
  
  # Buy
  tmp.b <- which(setup$compl == 1)
  if (length(tmp.b) > 0) {
    tmp <- which(setup$endsq == 1)
    rng <- rep(NA, length(tmp.b))
    for (i in 1: length(tmp.b)) {
      perf.b <- ohlc$Low[(tmp.b[i] - 1): (tmp[tmp >= tmp.b[i]][1] + 1)] < ohlc$Low[tmp.b[i] - lookback[1]] &
        ohlc$Low[(tmp.b[i] - 1): (tmp[tmp >= tmp.b[i]][1] + 1)] < ohlc$Low[tmp.b[i] - lookback[2]]
      setup$perf[(tmp.b[i] + which(perf.b) - 2)[1]] <- 1
    }
  }
  # Sell
  tmp.s <- which(setup$compl == -1)
  if (length(tmp.s) > 0) {
    tmp <- which(setup$endsq == -1)
    rng <- rep(NA, length(tmp.s))
    for (i in 1: length(tmp.s)) {
      perf.s <- ohlc$High[(tmp.s[i] - 1): (tmp[tmp >= tmp.s[i]][1] + 1)] > ohlc$High[tmp.s[i] - lookback[1]] &
        ohlc$High[(tmp.s[i] - 1): (tmp[tmp >= tmp.s[i]][1] + 1)] > ohlc$High[tmp.s[i] - lookback[2]]
      setup$perf[(tmp.s[i] + which(perf.s) - 2)[1]] <- 1
    }
  }
  # NOTE: tmp[tmp >= tmp.x[i]][1] + 1 extends it to the bar immediately after the end of the sequence where a L/H may occur and perfect the Setup
  
  return (setup$perf)
}

TD$ST <- function (ohlc, opt, setup, levels = c("true.hl", "hl")) {
  
  ## TD Setup Trend Levels (TDST)
  # References: 
  #             Jason Perl, DeMark Indicators; Bloomberg 2008, p. 5
  
  levels <- switch (levels,
            true.hl =  true.hl(ohlc),
            hl = cbind(ohlc$High, ohlc$Low))
  # resistance
  idx.tdst <- which(setup$compl == 1) - opt[2] + 1
  tmp <- matrix(NA, opt[3] + 1, length(idx.tdst))
  for (i in 0: opt[3]) tmp[i + 1, ] <- levels[idx.tdst - i, 1]
  tmp <- apply(tmp, 2, which.max)
  idx.tdst <- idx.tdst + 1 - tmp
  setup$tdst[idx.tdst] <- levels[idx.tdst, 1]
  # Support
  idx.tdst <- which(setup$compl == -1) - opt[2] + 1
  tmp <- matrix(NA, opt[3] + 1, length(idx.tdst))
  for (i in 0: opt[3]) tmp[i + 1, ] <- levels[idx.tdst - i, 2]
  tmp <- apply(tmp, 2, which.min)
  idx.tdst <- idx.tdst + 1 - tmp
  setup$tdst[idx.tdst] <- levels[idx.tdst, 2]
  # It is not significant if the TDST Level is violated on an intrabar basis but only on a closing basis
  
  return (setup$tdst)
}


TD$Risk <- function (ohlc, setup, opt, shift = c("TR", "ATR", "SDEV")) {
  
  ## TD Risk Levels
  #             Jason Perl, DeMark Indicators; Bloomberg 2008, pp. 14, 17

  #%%%  to test TD Risk Level using also perfection
  
  # Buy
  tmp.compl <- which(setup$compl == 1)
  if (length(tmp.compl) > 0) {
    for (i in tmp.compl) {
      lo <- which.min(ohlc$Low[i - (opt[2]: 1) + 1])
      if (is.numeric(shift)) setup$risk[i - (opt[2] - lo)] <- (1 - shift) * ohlc$Low[i - (opt[2] - lo)]
      else if (is.character(shift)) {
        if (shift == "SD") {
          setup$risk[i - (opt[2] - lo)] <- ohlc$Low[i - (opt[2] - lo)] - sd(ohlc$Close[i - (opt[2]: 1) + 1])
        } else {
          truehl.i <- true.hl(ohlc[i - (opt[2]: 1) + 1, ])
          truelo <- which.min(truehl.i[, 2])
          setup$risk[i - (opt[2] - truelo)] <- switch(shift,
                                                      TR  = 2 * truehl.i[truelo, 2] - truehl.i[truelo, 1],
                                                      ATR = truehl.i[truelo, 2] - mean(truehl.i[, 1] - truehl.i[, 2]))
        }
      }
    }
  }
  # Sell
  tmp.compl <- which(setup$compl == -1)
  if (length(tmp.compl) > 0) {
    for (i in tmp.compl) {
      hi <- which.max(ohlc$High[i - (opt[2]: 1) + 1])
      if (is.numeric(shift)) setup$risk[i - (opt[2] - lo)] <- (1 + shift) * ohlc$High[i - (opt[2] - hi)]
      else if (is.character(shift)) {
        if (shift == "SD") {
          setup$risk[i - (opt[2] - hi)] <- ohlc$High[i - (opt[2] - hi)] + sd(ohlc$Close[i - (opt[2]: 1) + 1])
        } else {
          truehl.i <- true.hl(ohlc[i - (opt[2]: 1) + 1, ])
          truehi <- which.max(truehl.i[, 2])
          setup$risk[i - (opt[2] - truehi)] <- switch(shift,
                                                      TR  = 2 * truehl.i[truehi, 1] - truehl.i[truehi, 2],
                                                      ATR = truehl.i[truehi, 1] + mean(truehl.i[, 1] - truehl.i[, 2]))
        }
      }
    }
  }
  
  return (setup$risk)
}


TD$Sequential <- function (ohlc, opt1 = c(2, 13, 8), opt2 = c(4, 9, 1), opt3 = c(1, 1, 1, 1), opt4 = c(0, 0, 1.618)) {

  # References: ?Tom Demark, The New Science of Technical Analysis; Wiley 1994, p. 153.
  #             ?New Market Timing Techniques; Wiley 1997, p. 43.
  #             ?DeMark on day-trading options; McGrow-Hill 1999, pp. 126, 158.
  #             Jason Perl, DeMark Indicators; Bloomberg 2008, pp. 17, 31.
  # Type:       Momentum indicator.
  # Value:      Trend exaustion point.
  #
  # Arguments:
  # ohlc - historical OHLC price time serie, as dataframe (with a date field) or xts/zoo object
  # opt1 - a set of default parameters customisable:
  #       [1] number of days to look back (2 by default).
  #       [2] number of counts for completion (13 by default).
  #       [3] previous count to validate completion (8 by default).
  # opt2 - parameters for TD Setup ([4, 9] by default).
  # opt3 - cancellation filters, 1 on, 0 off ([1, 1, 1, 1] by default):
  #       [1] TD Setup in opposite direction.
  #       [2] violation of the TDST of the current TD Setup.
  #       [3] cancellation qualifier I.
  #       [4] cancellation qualifier II.
  # opt4 - subset of parameters for cancellation filters
  #       %[1] relative to opt3[2], 0 if TDST is from TD Setup count 1, 1 if from TD Price Flip (0 by default).
  #       %[2] relative to opt3[2], 1 if TDST is using the true high or low, 0 if using high or low (1 by default).
  #       [3] realtive to opt3[3], ratio between the current TD Setup's true range and the previous TD Setup's true range (1.618 by default).
  #
  # Value:
  # returns dataframe
  # compl - numerical vector 1 for TD Buy Sequntial, -1 for TD Sell Sequential completion, 0 otherwise.
  # count - numerical vector counts from 1 to opt[2] (13 by default), positive for TD Buy Sequntial and negative for TD Sell Sequnrial.

  end <- dim(ohlc)[1]

  # Condition 1
  setup <- TD$Setup(ohlc, opt2)
  cond1.b <- which(setup$compl == 1)
  cond1.s <- which(setup$compl == -1)

  # Condition 2
  cond2.b <- ohlc$Close[(opt1[1] + 1): end] <= ohlc$Low[1: (end - opt1[1])]
  cond2.s <- ohlc$Close[(opt1[1] + 1): end] >= ohlc$High[1: (end - opt1[1])]
  
  # Counting
  col.b <- length(cond1.b)
  col.s <- length(cond1.s)
  countdown <- count <- matrix(0, end, col.b + col.s)
  # Buy
  for (i in 1: col.b) count[cond1.b[i]: end, i] <- 1
  count[(opt1[1] + 1): end, 1: col.b] <- count[(opt1[1] + 1): end, 1: col.b] * cond2.b
  # Sell
  for (i in 1: col.s) count[cond1.s[i]: end, col.b + i] <- -1
  count[(opt1[1] + 1): end, col.b + 1: col.s] <- count[(opt1[1] + 1): end, col.b + 1: col.s] * cond2.s
  count <- count * apply(abs(count), 2, cumsum)
  
  # Cancellation filter
  # References: 
  #             Jason Perl, DeMark Indicators; Bloomberg 2008, p. 21, 34
  canc1.b <- canc2.b <- rep(NA, col.b)
  canc1.s <- canc2.s <- rep(NA, col.s)
  truehl.ohlc <- true.hl(ohlc)
  # Buy
  for (i in 1: col.b) {
    # TD setup of opposite sign
    if (opt3[1] == 1) {
      tmp <- cond1.s[cond1.s > cond1.b[i]]
      canc1.b[i] <- ifelse(length(tmp) > 0, min(tmp), end)
    }
    # Violation of the TDST
    if (opt3[2] == 1 & cond1.b[i] < end) {
      tmp <- which(truehl.ohlc[(cond1.b[i] + 1): canc1.b[i], 2] > max(setup$tdst[cond1.b[i] - opt2[2] + 1 - c(0: opt2[3])]))[1]
      canc2.b[i] <- cond1.b[i] + tmp - 1
    }
  }
  canc.b <- pmin(canc1.b, canc2.b, na.rm = TRUE)
  # Sell
  for (i in 1: col.s) {
    # TD setup of opposite sign
    if (opt3[1] == 1) {
      tmp <- cond1.b[cond1.b > cond1.s[i]]
      canc1.s[i] <- ifelse(length(tmp) > 0, min(tmp), end)
    }
    # Violation of the TDST
    if (opt3[2] == 1 & cond1.s[i] < end) {
      tmp <- which(truehl.ohlc[(cond1.s[i] + 1): canc1.s[i], 1] < max(setup$tdst[cond1.s[i] - opt2[2] + 1 - c(0: opt2[3])]))[1]
      canc2.s[i] <- cond1.s[i] + tmp - 1
    }
  }
  canc.s <- pmin(canc1.s, canc2.s, na.rm = TRUE)
  
  
  # Cancellation qualifiers
  # References: 
  #             Jason Perl, DeMark Indicators; Bloomberg 2008, p. 21, 34
  if (opt3[3] + opt3[4] > 1) {
    tmp <- setup$compl[setup$compl != 0]
    tmp <- head(tmp, -1) - tail(tmp, -1)
    tmp <- tmp == 0
    first <- which(setup$compl != 0)[c(tmp, FALSE)]  # initial Buy/Sell TD Setup
    second <- which(setup$compl != 0)[c(FALSE, tmp)] # subsequent Buy/Sell TD Setup
    if (opt3[3] == 1) {
      # Cancellation qualifier I
      
  
    } else if (opt3[4] == 1) {
      # by cancellation qualifier II
    }
  }

  # A more conservative approach would also require the low of TD
  # Buy Countdown bar eight to be less than, or equal to, the close of TD
  # Buy Countdown bar five, but DeMark considers this an elective
  # option rather than a prerequisite.

  # Condition 3
  cond3.b <- ifelse(col.b > 1, unlist(apply(count[, 1: col.b], 2, function (x) {which(x == opt1[3])})), which(count[, 1] == opt1[3]))
  cond3.s <- ifelse(col.s > 1, unlist(apply(count[, col.b + 1: col.s], 2, function (x) {which(x == -opt1[3])})), which(col.b + 1 == -opt1[3]))
  
  # Completion
  # Buy
  len.b <- ifelse(tail(cond3.b, 1) == tail(canc.b, 1), length(cond3.b) - 1, length(cond3.b))
  for (i in 1: len.b) {
    count[canc.b[i]: end, i] <- 0
    if (cond3.b[i] < canc.b[i]) {
      tmp.b <- ohlc$Low[(cond3.b[i] + 1): (canc.b[i] - 1)] <= ohlc$Close[cond3.b[i]]
      tmp.b <- count[(cond3.b[i] + 1): (canc.b[i] - 1), i] > 0 & !tmp.b
      count[(cond3.b[i] + 1): (canc.b[i] - 1), i][tmp.b] <- 0.1 # '+'
      tmp.b <- count[(cond3.b[i] + 1): (canc.b[i] - 1), i] >= 1
      count[(cond3.b[i] + 1): (canc.b[i] - 1), i][tmp.b] <- opt1[3] + (1: sum(tmp.b))
    }
    if (any(count[, i] == opt1[2])) countdown[1: which(count[, i] == opt1[2]), i] <- count[1: which(count[, i] == opt1[2]), i] else countdown[, i] <- 0
  }
  if (length(cond3.b) != col.b) countdown[, (length(cond3.b) + 1): col.b] <- count[, (length(cond3.b) + 1): col.b] # ongoing count
  # Sell
  len.s <- ifelse(tail(cond3.s, 1) == tail(canc.s, 1), length(cond3.s) - 1, length(cond3.s))
  for (i in 1: len.s) {
    count[(canc.s[i]): end, col.b + i] <- 0
    if (cond3.s[i] < canc.s[i]) {
      tmp.s <- ohlc$High[(cond3.s[i] + 1): (canc.s[i] - 1)] >= ohlc$Close[cond3.s[i]]
      tmp.s <- count[(cond3.s[i] + 1): (canc.s[i] - 1), col.b + i] < 0 & !tmp.s
      count[(cond3.s[i] + 1): (canc.s[i] - 1), col.b + i][tmp.s] <- -0.1
      tmp.s <- count[(cond3.s[i] + 1): (canc.s[i] - 1),  col.b + i] <= -1
      count[(cond3.s[i] + 1): (canc.s[i] - 1),  col.b + i][tmp.s] <- -opt1[3] - (1: sum(tmp.s))
    }
    if (any(count[, col.b + i] == -opt1[2])) countdown[1: which(count[,  col.b + i] == -opt1[2]),  col.b + i] <- count[1: which(count[,  col.b + i] == -opt1[2]),  col.b + i] else countdown[, i] <- 0
  }
  if (length(cond3.s) != col.s) countdown[, (col.b + (length(cond3.s) + 1): col.s)] <- count[, (col.b + (length(cond3.s) + 1): col.s)] # ongoing count
  
  # countdown <- data.frame(ohlc$Date, countdown)

  return (countdown)

  # Risk Management
  #             Jason Perl, DeMark Indicators; Bloomberg 2008, pp. 26, 39.
  
  
  # TD Sequential 9-13-9, qualified if completion of bar 13 is before the start of the 2nd setup
  
  # TD Aggressive Sequential pg. 58
}
  # Two Ways to Enter a Long Position
  # Aggressive Approach: Buy on the close of a TD Buy Countdown bar thirteen, or
  # Conservative Approach: Subsequent to a TD Sequential Buy Countdown thirteen, wait
  # for the first instance when the close is greater than the close four price bars earlier—
  # i.e., a bullish TD Price Flip. 
  
  
  ## NOTES
    #  cancellation by violation of the TDST with true low/high instead of
    #  close


TD$Combo <- function (ohlc, opt1 = c(2, 1, 1, 1, 13, 1, 10), opt2 = c(4, 9, 1)) {
  
  # References: New Market Timing Techniques; Wiley 1997, p. 119.
  #             DeMark on day-trading options; McGrow-Hill 1999, pp. 168.
  #             Jason Perl, DeMark Indicators; Bloomberg 2008, pp. 60, 64.
  # Type:       Momentum indicator.
  # Value:      Trend exaustion point.
  #
  # Arguments:
  # ohlc - historical OHLC price time serie, as dataframe (with a date field) or xts/zoo object
  # opt1 - a set of default parameters customisable:
  #       [1] number of days to look back for close to low comparison (2 by default).
  #       [2] number of days to look back for low to low comparison (1 by default).
  #       [3] number of days to look back for close to close comparison (1 by default).
  #       [4] number of days to look back for TD Combo Countdown close to close comparison (1 by default).
  #       [5] number of counts for completion (13 by default).
  #       [6] 0 for TD Combo Countdown version I (strict), 1 for version II (less strict) (0 by default).
  #       [7] bar count after which version II applies.
  #
  # Value:
  # returns dataframe
  # compl - numerical vector 1 for TD Buy Sequntial, -1 for TD Sell Sequential completion, 0 otherwise.
  # count - numerical vector counts from 1 to opt[2] (13 by default), positive for TD Buy Sequntial and negative for TD Sell Sequnrial.
  
  end <- dim(ohlc)[1]
  
  # Condition 1
  setup <- TD$Setup(ohlc, opt2)
  cond1.b <- which(setup$compl == 1) - opt2[2] + 1
  cond1.s <- which(setup$compl == -1) - opt2[2] + 1
  
  # Condition 2
  cond2.b <- ohlc$Close[(opt1[1] + 1): end] <= ohlc$Low[1: (end - opt1[1])]
  cond2.s <- ohlc$Close[(opt1[1] + 1): end] >= ohlc$High[1: (end - opt1[1])]
  
  # Condition 3
  cond3.b <- ohlc$Low[(opt1[2] + 1): end] <= ohlc$Low[1: (end - opt1[2])]
  cond3.s <- ohlc$High[(opt1[2] + 1): end] >= ohlc$High[1: (end - opt1[2])]
  
  # Condition 4
  cond4.b <- ohlc$Close[(opt1[3] + 1): end] <= ohlc$Close[1: (end - opt1[3])]
  cond4.s <- ohlc$Close[(opt1[3] + 1): end] >= ohlc$Close[1: (end - opt1[3])]
  
  # Counting
  rm <- max(opt1[1: 4]) - c(0, opt1[1: 4]) # remove data points to allign all vectors for conditions 2, 3, 4
  cond.b <- cond2.b[(rm[2] + 1): length(cond2.b)] * cond3.b[(rm[3] + 1): length(cond3.b)] * cond4.b[(rm[4] + 1): length(cond4.b)]
  cond.s <- cond2.s[(rm[2] + 1): length(cond2.s)] * cond3.s[(rm[3] + 1): length(cond3.s)] * cond4.s[(rm[4] + 1): length(cond4.s)]
  col.b <- length(cond1.b)
  col.s <- length(cond1.s)
  count <- matrix(0, end, col.b + col.s)
  # Buy
  for (i in 1: col.b) count[cond1.b[i]: end, i] <- 1
  count[(rm[1] + 1): end, 1: col.b] <- count[(rm[1] + 1): end, 1: col.b] * cond.b
  # Sell
  for (i in 1: col.s) count[cond1.s[i]: end, col.b + i] <- -1
  count[(rm[1] + 1): end, col.b + 1: col.s] <- count[(rm[1] + 1): end, col.b + 1: col.s] * cond.s
  countdown <- count
  
  # Condition 5
  for (i in 1: col.b) {
    cond5.b <- ohlc$Close * count[, i]
    tmp <- which(cond5.b > 0)
    tmp2 <- c()
    for (j in (1 + opt1[4]): length(tmp)) if (cond5.b[tmp[j]] > min(cond5.b[tmp[1: (j - 1)]])) countdown[tmp[j], i] <- 0
  }
  for (i in 1: col.s) {
    cond5.s <- ohlc$Close * count[, col.b + i]
    tmp <- which(cond5.s < 0)
    for (j in (1 + opt1[4]): length(tmp)) if (cond5.s[tmp[j]] > min(cond5.s[tmp[1: (j - 1)]])) countdown[tmp[j], i] <- 0
  }

  countdown <- countdown * apply(abs(countdown), 2, cumsum)

  # Version II
  if (opt1[6] == 1) {
    ver2 <- apply(abs(countdown), 2, function (x) {which(x == opt1[7])})
    ver2[lengths(ver2) == 0L] <- NA
    ver2 <- unlist(ver2) + 1
    tmp <- (1: col.b)[!is.na(ver2[1: col.b])]
    for (i in tmp) countdown[ver2[i]: (end - opt1[4]), i] <- (cumsum(cond4.b[ver2[i]: (end - opt1[4])]) + opt1[7]) * cond4.b[ver2[i]: (end - opt1[4])]
    tmp <- (col.b + 1: col.s)[!is.na(ver2[col.b + 1: col.s])]
    for (i in tmp) countdown[ver2[i]: (end - opt1[4]), i] <- -(cumsum(cond4.b[ver2[i]: (end - opt1[4])]) + opt1[7]) * cond4.b[ver2[i]: (end - opt1[4])]
  }
  
  countdown[abs(countdown) > opt1[5]] <- 0
  countdown <- data.frame(ohlc$Date, countdown)



  # Version II
  if (opt1[6] == 1) {
    ver2 <- apply(abs(countdown), 2, function (x) {which(x == opt1[7])})
    ver2[lengths(ver2) == 0L] <- NA
    ver2 <- unlist(ver2)
    tmp <- (1: col.b)[!is.na(ver2[1: col.b])]
    for (i in tmp) countdown[ver2[i]: end, i] <- cond4.b[ver2[i]: end - opt1[4]]
    tmp <- (col.b + 1: col.s)[!is.na(ver2[col.b + 1: col.s])]
    for (i in tmp) countdown[ver2[i]: end, i] <- -cond4.s[ver2[i]: end - opt1[4]]
  }

  countdown <- count * apply(abs(count), 2, cumsum)
  countdown[abs(countdown) > opt1[5]] <- 0
  countdown <- data.frame(ohlc$Date, countdown)
  
}



TD$Dwave <- function (ohlc, wave = c(21, 13, 8, 21, 13, 34, 13, 8, 21), opt = c(1, 1, 1, 1, 1, 1, 1), cont = TRUE, proj = TRUE)  { # wave = c(21, 8, 5, 13, 8, 21, 13, 8, 21)
  
  # References: Tom Demark, New Market Timing Techniques; Wiley 1997, p. 101.
  #             ? DeMark on day-trading options; McGrow-Hill 1999, pp. 168.
  #             Jason Perl, DeMark Indicators; Bloomberg 2008, pp. 60, 64.
  
  # Wave 1. Sentiment is overwhelmingly bearish. Bullish divergence between price action and momentum.
  # Wave 2. At least 61.8% retracement but cannot trade below the low of wave 1.
  # Wave 3. Open interest,volume, and price action should all pick up dramatically in the direction of the developing uptrend.
  # Wave 4. Price action is volatile. 38.2% of wave 3’s gains, 61.8 percent or more if wave 2 was  very shallow. High of 1 is less than the low of 4.
  # Wave 5. Sentiment is overwhelmingly bullish. Bearish divergence between price action and momentum.
  # Wave A. Economic news remains generally supportive of the broader uptrend, views the setback as a healthy correction.
  # Wave B. The attempted rally from the low of wave A fails to overcome the prior wave-5 peak.
  # Wave C. Move lower is impulsive and its length will tend to be 1.618 times that of wave A.
  
  end <- dim(ohlc)[1]
  
  wave.cond <- unique(wave)
  cond.dn <- cond.up <- matrix(0, end, length(wave.cond))
  for (i in 1: length(wave.cond)) {
    for (j in (wave.cond[i] + 1): end) {
      cond.up[j, i] <- all(ohlc$Close[(j - wave.cond[i]): (j - 1)] < ohlc$Close[j])
      cond.dn[j, i] <- all(ohlc$Close[(j - wave.cond[i]): (j - 1)] > ohlc$Close[j])
    }
  }
  loop <- 0
  # Wave 0
  w0 <- which(cond.dn[, which(wave.cond == wave[1])] == 1)
  w1 <- which(cond.up[, which(wave.cond == wave[2])] == 1)
  w1 <- w1[w1 >= (w0[1] + wave[2])]
  w0 <- w0[w0 <= w1[1]]
  w0 <- w0[which.min(ohlc$Close[w0])]
  w1 <- w1[w1 >= (w0 + wave[2])]
  # wave 1
  w2 <- which(cond.dn[, which(wave.cond == wave[3])] == 1)
  w2 <- w2[w2 >= (w1[1] + wave[3])]
  w1 <- w1[w1 <= w2[1]]
  w1 <- w1[which.max(ohlc$Close[w1])]
  w2 <- w2[w2 >= (w1 + wave[3])]
  # wave 2
  w3 <- which(cond.up[, which(wave.cond == wave[4])] == 1)
  w3 <- w3[w3 >= (w2[1] + wave[4])]
  w2 <- w2[w2 <= w3[1]]
  w2 <- w2[which.min(ohlc$Close[w2])]
  if (ohlc$Low[w2] < ohlc$Low[w0]) loop <- 1 # wave 2 must not make new lows
  w3 <- w3[w3 >= (w2 + wave[4])]
  # wave 3
  w4 <- which(cond.dn[, which(wave.cond == wave[5])] == 1)
  w4 <- w4[w4 >= (w3[1] + wave[5])]
  w3 <- w3[w3 <= w4[1]]
  w3 <- w3[which.max(ohlc$Close[w3])]
  if (ohlc$High[w3] >= ohlc$High[w1]) loop <- 1 # wave 3 must make a new high
  w4 <- w4[w4 >= (w3 + wave[5])]
  # wave 4
  w5 <- which(cond.up[, which(wave.cond == wave[6])] == 1)
  w5 <- w5[w5 >= (w4[1] + wave[6])]
  w5 <- w5[ohlc$High[w5] >= ohlc$High[w3]]
  w4 <- w4[w4 <= w5[1]]
  w4 <- w4[which.min(ohlc$Close[w4])]
  if (ohlc$Low[w4] >= ohlc$High[w1]) loop <- 1 # wave 4 must not make a low lower than wave 1 high
  w5 <- w5[w5 >= (w4 + wave[6])]
}


TD$Lines <- function () {}
  
  # unexpected trendline violations often overshoot in the direction of the break
  # short-term breakouts tend to not follow through immediately on trendline violations that have been widely anticipated
  
  # TD Demand Line Qualifiers
  # 1. Close of bar [X-1] is higher than the close of bar [X-2] for prior to the breakout, so the intrabar downside violation of the TD Demand Line is qualified
  # 2. Market opens below the TD Demand Line
  # 3. Calculate the difference between bar's [X-1] close and the greater of either a) that bar’s high or b) the previous bar’s close (close of X-2) and Subtract that value from the close of X - 1
  
  # Objective for a Qualified Downside Violation
  # 1. identify the highest true high above the TD Demand Line, and drop a perpendicular line to the corresponding TD Demand Line below it.
  # 2. Subtract that value from the point where the qualified breakout occurred.
  
  # Exiting a Short Position Using TD Demand Line
  # 1. if the open of the bar immediately after the downside breakout is above the
  # TD Demand Line breakout level, exit at the open.
  # 2. if the open of the bar immediately after the downside breakout is above the
  # TD Demand Line, and the bar closes above the TD Demand Line breakout level,
  # exit at the close.
  # 3. if the low of the bar following the TD Demand Line break fails to record a lower
  # low than the low of the breakout bar, exit at the close.

true.hl <- function (ohlc) {

  # References: Tom Demark, The New Science of Technical Analysis; Wiley 1994, p. 15
  # TRUEHL returns two vectors of highs and lows that fill any price gap from the previous close.
  #
  # Arguments:
  # H - high prices
  # L - low prices
  # C - close prices
  #
  # Value:
  # trueh - numerical vector; true highs.
  # truel - numerical vector; true lows.
  
  end <- dim(ohlc)[1]
  trueh <- c(0, (ohlc$Close[1: (end - 1)] > ohlc$High[2: end]) * ohlc$Close[1: (end - 1)])
  trueh[trueh == 0] <- ohlc$High[trueh == 0]
  truel <- c(0, (ohlc$Close[1: (end - 1)] < ohlc$Low[2: end]) * ohlc$Close[1: (end - 1)])
  truel[truel == 0] <- ohlc$Low[truel == 0]
  
  return (cbind(trueh, truel))
}


TD$SetupTrend <- function () {}
  
  # Name:       TD Setup Trend (TDST).
  # Author:     Tom R. DeMark.
  # References: New Market Timing Techniques; Wiley 1997, p. 58.
  #             DeMark on day-trading options; McGrow-Hill 1999, p. 206.
  #             Jason Perl, DeMark Indicators; Bloomberg 2008, pp. 6, 9.
  # Type:       Breakout.
  # Value:      Price levels as support / resistance for TD Setup, and
  #             cancellation for TD Countdown.
  #
  # Notes: the violation of the TDST is validated by the close. An opening
  #        gap the next day may invalidate the breakout. If within the next
  #        three days a new high or low is not made from the violation day, 
  #        the breakout is invalidated.
  #
  # INPUTS:
    # setup - completion of TD Setup (compl, in tdsetup.m)
  # H - high prices
  # L - low prices
  # C - close prices
  # opt - a set of default parameters customisable:
    #   (1) - 0 if the start is from TD Setup count 1, 1 if from TD Price Flip
  #       (0 by default). 
  #   (2) - 1 if using the true high or low for opt(1), 0 if using high or 
  #       low (1 by default).
  # opt2 - parameters for TD Setup ([4, 9] by default).
  #
  # OUTPUTS:
    # level - price level of the support or resistance.
  # status - 1 if support, -1 if resistance.
  # start - ordinal vector of starts.
  # end - ordinal vector of ends, blank cells when the level is still active.
  

  ## NOTES
  # terminate when setup of opposite sign
  #  for further study make endl more flexible rather than using just close.
  
  
  ## Qualified Sequential 9 13 9 Buy Signal
  ## Recycle
  
  
  # TD Retracements
  # to determine which reference low to use when projecting retracements from a reference high, determine when the market
  # last traded at the reference high, and select the lowest point between the two highs (TD Magnet Price).
  # ratios 0.382 and 0.618, TD Relative Retracement ratios 1.382, 1.618, 2.236 and 2.618
  # It is because prices are drawn to the close of the reference high or reference low that DeMark refers to this level as the TD Magnet Price.
  
  # Three Conditions for Validating a TD Relative Retracement Level:
  #   Only One Needs To Be Satisfied for a Qualified Break
  # 1. The close of the price bar one bar before an intrabar upside break
  # of a TD relative retracement level must be below the closing price two bars before
  # the intrabar upside break.
  # 2. if condition one isn’t satisfied, then a qualifying break can still occur,
  # if the market opens above a TD relative retracement level and then trades one tick
  # above the open.
  # 3. if the difference between the close of the bar prior to the upside
  # break and its true low (the lesser of that bar’s low or the previous close) is calculated,
  # and value of the close of the price bar preceding the upside break is added to that,
  # the result must be less than the TD relative retracement level.
  # 
  # Conditions That Invalidate an Upside Break of a TD Relative Retracement Level
  # 1. if the open of the price bar following a qualified upside break is
  # below the TD relative retracement level, exit the long position on the open.
  # 2. if the open of the bar following a qualified upside break is below the
  # close of the breakout bar, and then it closes below the TD relative retracement level,
  # exit the long position on the close.
  # 3. if the high of the bar following the qualified upside break is below the
  # high of the breakout bar, exit the long position on the close.



require (quantmod)

data <- ohlc
OHLC <- xts(data[, 2: 5], order.by = data[, 1])

OHLC <- to.weekly(OHLC)
#OHLC <- to.monthly(OHLC)
colnames(OHLC) <- c("Open", "High", "Low", "Close")
# colnames(OHLC)[2: 5] <- c("Open", "High", "Low", "Close") # intraday
data <- data.frame(Date = as.Date(index(OHLC)), OHLC)

chartSeries(OHLC, type = "candle", theme = 'white')

# Setup
opt <- c(4, 9, 1)
setup <- xts(TD$Setup(data, opt), order.by = data[, 1])
for (i in 1: opt[2]) {
  tmp <- which(setup$setup == i)
  addPoints(x = tmp, y = OHLC$Low[tmp] * 0.99, pch = 48 + i, cex = 150 / nrow(setup), col = 'green')
  tmp <- which(setup$setup == -i)
  addPoints(x = tmp, y = OHLC$High[tmp] * 1.01, pch = 48 + i, cex = 150 / nrow(setup), col = 'green')
}
# Perfection
addPoints(x = which(setup$perf == 1 & sign(setup$setup) < 0), y = OHLC$High[which(setup$perf == 1 & sign(setup$setup) < 0)] * 1.035, pch = 25, cex = 200 / nrow(setup), col = 'green')
addPoints(x = which(setup$perf == 1 & sign(setup$setup) > 0), y = OHLC$Low[which(setup$perf == 1 & sign(setup$setup) > 0)] * 0.965, pch = 24, cex = 200 / nrow(setup), col = 'green')

# Sequential
opt1 <- c(2, 13, 8)
sequential <- xts(TD$Sequential(data[, 2: 5], opt1), order.by = data[, 1])
for (i in 1: opt1[2]) {
  if (i <= 9) {
    tmp <- which(sequential == i, arr.ind = TRUE)[, 1]
    addPoints(x = tmp, y = OHLC$Low[tmp] * 0.98, pch = 48 + i, cex = 150 / nrow(sequential), col = 'magenta')
    tmp <- which(sequential == -i, arr.ind = TRUE)[, 1]
    addPoints(x = tmp, y = OHLC$High[tmp] * 1.02, pch = 48 + i, cex = 150 / nrow(sequential), col = 'magenta')
  } else if (i <= 99) {
    tmp <- which(sequential == i, arr.ind = TRUE)[, 1]
    addPoints(x = tmp - 1, y = OHLC$Low[tmp] * 0.98, pch = 48 + as.integer(i / 10), cex = 150 / nrow(sequential), col = 'magenta')
    addPoints(x = tmp, y = OHLC$Low[tmp] * 0.98, pch = 48 + i %% 10, cex = 150 / nrow(sequential), col = 'magenta')
    tmp <- which(sequential == -i, arr.ind = TRUE)[, 1]
    addPoints(x = tmp - 1, y = OHLC$High[tmp] * 1.02, pch = 48 + as.integer(i / 10), cex = 150 / nrow(sequential), col = 'magenta')
    addPoints(x = tmp, y = OHLC$High[tmp] * 1.02, pch = 48 + i %% 10, cex = 150 / nrow(sequential), col = 'magenta')
  }
}



https://stackoverflow.com/questions/41088664/how-to-highlight-individual-candles-in-a-quantmod-chart
addPoints(x = 1:nrow(AAPL["2018-08"]), y = AAPL["2018-08", 6])
addLines(v=which(index(dataxts) == as.POSIXlt("2010-12-29 00:11:04", tz="GMT")))


blp <- blpConnect(); ohlc <- bdh("EUR Curncy", c("PX_OPEN", "PX_HIGH", "PX_LOW", "PX_LAST"), Sys.Date() - 365 * 1, Sys.Date()); colnames(ohlc) <- c("Date", "Open", "High", "Low", "Close")

ohlc <- getBars("IBE SM Equity", barInterval = 3, startTime = Sys.time() - 60 * 60 * 10)
colnames(ohlc)[2: 5] <- c("Open", "High", "Low", "Close")
