### Relative Rotation Graph

require (quantmod)


# # SPDR sector ETFs
# symbols <- c(
#   "SPY", #SPDR            
#   "XLY", #SPDR Consumer discretionary sector
#   "XLP", #SPDR Consumer staples sector             
#   "XLE", #SPDR Energy sector
#   "XLF", #SPDR Financial sector
#   "XLV", #SPDR Health Care sector
#   "XLI", #SPDR Industrials sector
#   "XLK", #SPDR Technology sector
#   "XLB", #SPDR Materials sector
#   #"XLRE",#SPDR Real Estate sector
#   #"XLC", #SPDR Communication services sector
#   "XLU"  #SPDR Utilities sector
# )
# 
# getSymbols(symbols, from = "1990-01-01")
# prices <- list()
# for(i in 1: length(symbols)) {
#     prices[[i]] <- Ad(get(symbols[i]))
# }
# prices <- do.call(cbind, prices)
# colnames(prices) <- gsub("/.[A-z]*", "", colnames(prices))
# returns <- Return.calculate(prices)
#
# # Bloomberg
# require (Rblpapi)
# blp <- blpConnect()
# prices <- bdh(paste(symbols, "US Equity"), c("PX_LAST"), Sys.Date() - 365 * 10, Sys.Date())

symbols <- c(
  "SPX",    #S&P 500 Index
  "S5COND", #S&P 500 Consumer Discretionary Sector GICS
  "S5CONS", #S&P 500 Consumer Staples	Sector GICS
  "S5ENRS", #S&P 500 Energy	Sector GICS
  "S5FINL", #S&P 500 Financial Sector GICS
  "S5HLTH", #S&P 500 Health Care Sector GICS
  "S5INDU", #S&P 500 Industrials Sector GICS
  "S5INFT", #S&P 500 Information Technology Sector GICS
  "S5MATR", #S&P 500 Materials Sector GICS
  "S5RLST", #S&P 500 Real Estate Sector GICS
  "S5TELS", #S&P 500 Communication Services	Sector GICS
  "S5UTIL"  #S&P 500 Utilities Sector GICS
)

# Bloomberg
require (Rblpapi)
blp <- blpConnect()
memo <- bdh(paste(symbols, " Index"), c("PX_LAST"), as.Date("1900-01-01"), Sys.Date())
prices <- memo
start.date <- as.Date(rep(0, length(prices)))
for (i in 1: length(prices)) {
  prices[[i]] <- xts(prices[[i]][, 2], order.by = as.Date(prices[[i]][, 1]))
  start.date[i] <- head(index(na.omit(prices[[i]])), 1)
}
prices <- do.call(cbind, prices)
header <- sapply(colnames(prices), function (x) strsplit(x, "..", fixed = TRUE)[[1]][1])
prices <- prices[, match(symbols, header)]
start.date <- start.date[match(symbols, header)]
colnames(prices) <- symbols

# relative prices
rel <- prices[index(prices) >= max(min(start.date[-1]), start.date[1]), -1]
for (i in 1: dim(rel)[2]) rel[, i] <- rel[, i] / prices[, 1]
rel.lret <- diff(log(rel))

require (TTR)
ma <- c("EMA", "EMA", "EMA")
par <- c(10, 30, 9)
data.set <- NULL
for (i in 1: dim(rel)[2]) {
  macd <- MACD(rel[, i], par[1], par[2], 9, maType = list(list(ma[1]), list(ma[2]), list(ma[3])), percent = TRUE)
  macd$mom <- macd[, 1] - macd[, 2]
  data.set <- rbind(data.set, data.frame(index(macd), macd[, 1], macd[, 3], sqrt(diff(macd[, 1]) ^2 + diff(macd[, 3]) ^2), diff(macd[, 3]) / diff(macd[, 1]), colnames(rel)[i]))
}
colnames(data.set) <- c("date", "strength", "momentum", "velocity", "gradient", "sector")

# Analusing the features
library(ggplot2)
# Strength
ggplot(data.set, aes(x = strength)) +
       geom_histogram(binwidth = 0.2) + facet_grid(~sector) + theme_bw()
by(data.set$strength, data.set$sector, summary)
ggplot(data.set, aes(x = momentum)) +
  geom_histogram(binwidth = 0.1) + facet_grid(~sector) + theme_bw()
ggplot(data.set, aes(x = velocity)) +
  geom_histogram(binwidth = 0.05) + facet_grid(~sector) + theme_bw()

require (fitdistrplus)
test <- na.omit(data.set)
by(test$strength, test$sector, descdist)


end <- Sys.Date() - 1
start <- Sys.Date() - 42
sample <- data.set$date > start & data.set$date < end
colours <- rainbow(length(symbols) - 1)

plot(data.set$strength[sample], data.set$momentum[sample], type = 'n', 
     main = "Phase State", xlab = "Relative Strength", ylab = "Momentum")
limits = par()$usr
rect(0, 0, limits[2], limits[4], col = rgb(220, 255, 200, maxColorValue = 255))
rect(0, 0, limits[1], limits[4], col = rgb(220, 255, 255, maxColorValue = 255))
rect(0, 0, limits[1], limits[3], col = rgb(255, 220, 220, maxColorValue = 255))
rect(0, 0, limits[2], limits[3], col = rgb(255, 255, 200, maxColorValue = 255))
text(c(limits[2: 1]) * c(0.8, 0.9, 0.8, 0.9), c(limits[4], limits[4], limits[3], limits[3]) * 0.9, 
     labels = c("Leading", "Improving", "Weakening", "Lagging"), col = c("green", "blue", "orange", "red"))
col = 1
for (i in unique(data.set$sector)) {
  lines(xspline(data.set$strength[sample & data.set$sector == i], data.set$momentum[sample & data.set$sector == i], 1, draw = FALSE), lwd = 2, col = colours[col])
  points(data.set$strength[sample & data.set$sector == i], data.set$momentum[sample & data.set$sector == i], pch = 1, col = colours[col], cex = 0.8)
  points(data.set$strength[data.set$date == end & data.set$sector == i], data.set$momentum[data.set$date == end & data.set$sector == i], pch = 16, col = colours[col], cex = 1.3)
  col = col + 1
}
text(data.set$strength[data.set$date == end], data.set$momentum[data.set$date == end], labels = symbols[-1], pos = 4, col = 'black', cex = 0.8)
legend("top", legend = c("Cons. Disc.", "Cons. Stap.", "Energy", "Financial", "Health Care", "Industrials", "Technology",
                         "Materials", "Real Estate", "Comm. Serv.", "Utilities"), 
       lty = 1, col = colours, cex = 0.6)

require (gganimate)
require (gifski)
require (png)

# Static Plot
p <- ggplot(data.set[data.set$date > start.date[10] + 40, ],
            aes(x = strength, y = momentum, colour = sector)) +
     geom_point(show.legend = TRUE, size = 5) +
     xlim(c(-5, 5)) + ylim(c(-2.5, 2.5)) +
     scale_colour_viridis_d() +
     labs(x = "Relative Strength", y = "Relative Momentum")
p

# Transition through phase state
p + transition_time(date) + labs(title = "Year: {frame_time}") + view_follow(fixed_y = TRUE)
# Transition through phase state by sector

animate(p + facet_wrap(~sector) + transition_time(date) + labs(title = "Date: {frame_time}") + shadow_wake(wake_length = 0.02,  alpha = FALSE), detail = 5, fps = 4, nframes = 502)
anim_save("Sector rotation individual.gif", animation = last_animation())
animate(p + transition_time(date) + labs(title = "Date: {frame_time}") + shadow_wake(wake_length = 0.02, alpha = FALSE), detail = 5, fps = 4, nframes = 502)
anim_save("Sector rotation.gif", animation = last_animation())


fit.ellipse <- function (x, y = NULL) {
  
  # Least squares fitting of an ellipse to point data using the algorithm described in: 
  # Radim Halir & Jan Flusser - Numerically stable direct least squares fitting of ellipses. 
  # Proceedings of the 6th International Conference in Central Europe on Computer Graphics and Visualization. WSCG '98, p. 125-132 
  #
  # Arguments
  # x, y    coordinates of the data points, passed as a two column patrix (x) or as two vectors (x, y)
  #
  # Value: 
  # coef    coefficients of the ellipse: ax^2 + bxy + cy^2 + dx + ey + f = 0
  # center  coordinates x, y
  # axes    radius of the two axes
  # angle   rotation of the axes in radiants
  # eof     error of fit, approximated as the algebraic distance (better if inversely weighted by its gradient)
  
  EPS <- 1.0e-8
  dat <- xy.coords(x, y)
  D1 <- cbind(dat$x * dat$x, dat$x * dat$y, dat$y * dat$y)
  D2 <- cbind(dat$x, dat$y, 1)
  S1 <- t(D1) %*% D1
  S2 <- t(D1) %*% D2
  S3 <- t(D2) %*% D2
  T <- -solve(S3) %*% t(S2)
  M <- S1 + S2 %*% T
  M <- rbind(M[3, ] / 2, -M[2, ], M[1, ] / 2)
  evec <- eigen(M)$vec
  cond <- 4 * evec[1, ] * evec[3, ] - evec[2, ] ^2
  a1 <- evec[, which(cond > 0)]
  f <- c(a1, T %*% a1)
  names(f) <- letters[1: 6]

  # calculate the center and the radius of the axes
  # the center is the solution to the pair of equations
  # 2ax + by + d = 0
  # bx + 2cy + e = 0

  A <- matrix(c(2 * f[1], f[2], f[2], 2 * f[3]), nrow = 2, ncol = 2, byrow = TRUE)
  b <- matrix(c(-f[4], -f[5]), nrow = 2, ncol = 1, byrow = TRUE)
  soln <- solve(A) %*% b
  b2 <- f[2] ^2 / 4

  center <- c(soln[1], soln[2])
  names(center) <- c("x", "y")

  num  <- 2 * (f[1] * f[5] ^2 / 4 + f[3] * f[4] ^2 / 4 + f[6] * b2 - f[2] * f[4] * f[5]/4 - f[1] * f[3] * f[6])
  den1 <- (b2 - f[1] * f[3])
  den2 <- sqrt((f[1] - f[3]) ^2 + 4 * b2)
  den3 <- f[1] + f[3]
  axes <- sqrt(c(num / (den1 * (den2 - den3)), num / (den1 * (-den2 - den3))))

  # calculate the angle of rotation
  term <- (f[1] - f[3]) / f[2] 
  angle <- sign(term) * atan(1 / term) / 2
  
  eof <- cbind(D1, D2) %*% f
  
  return (list(coef = f, center = center, axes = range(axes), angle = unname(angle), eof = eof))
}

get.ellipse <- function (fit, n = 360) {

  # Arguments:
  # fit     output list of parameters from fit.ellipse()
  # x       
  # n is the number of points to render

  tt <- seq(0, 2 * pi, length.out = n)
  sa <- sin(fit$angle)
  ca <- cos(fit$angle)
  ct <- cos(tt)
  st <- sin(tt)

  x <- fit$center[1] + fit$axes[2] * ct * ca - fit$axes[1] * st * sa
  y <- fit$center[2] + fit$axes[2] * ct * sa + fit$axes[1] * st * ca

  return (cbind(x = x, y = y))
}

create.ellipse <- function (rx = 300,         # x axis radius
                            ry = 200,         # y axis radius
                            cx = 250,         # x center
                            cy = 150,         # y center
                            angle = 0.4,      # radians
                            sigma = 25,       # error as sd of Gaussian noise
                            n = 30)           # number of points
{
  set.seed(1234)
  t <- seq(0, 2 * pi, length.out = n)
  x <- rx * cos(t)
  y <- ry * sin(t)
  nx <- x * cos(angle) - y * sin(angle) + cx
  nx <- nx + rnorm(length(t)) * sigma
  ny <- x * sin(angle) + y * cos(angle) + cy
  ny  <- ny + rnorm(length(t)) * sigma
  cbind(x = nx, y = ny)
}

X <- create.ellipse()
efit <- fit.ellipse(X)
e <- get.ellipse(efit)
plot(X) 
lines(e, col = "red") 

print(efit)

X <- cbind(data.set$strength[120: 150], data.set$momentum[120: 150])
efit <- fit.ellipse(X)
e <- get.ellipse(efit)
plot(X, xlim = c(-2, 2), ylim = c(-1, 1)) 
lines(e, col = "red") 

# fitting the elliptical
n.points <- 6: 30
fit.ellipse <- vector(mode = "list", length = length(n.points))
for (i in 1: length(fit.ellipse)) {
  fit.ellipse[[i]] <- vector(mode = "list", length = length(symbols[-1]))
  names(fit.ellipse[[i]]) <- symbols[-1]
}





require (PerformanceAnalytics)

# Sell
# cond1 - Leading
cond1.s <- rs.ratio > 0 & rs.momentum > 0
# cond2 - Inflexion of RS
cond2.s <- rs.ratio < apply(rs.ratio, 2, runMax, n = 20)
# cond3 - Decreasing Momentum
cond3.s <- rbind(NA, rs.ratio.diff) < 0
sig.s <- cond1.s * cond2.s * cond3.s

# Buy
# cond1 - Lagging
cond1.b <- rs.ratio < 0 & rs.momentum < 0
# cond2 - Inflexion of RS
cond2.b <- rs.ratio > apply(rs.ratio, 2, runMin, n = 20)
# cond3 - Decreasing Momentum
cond3.b <- rbind(NA, rs.ratio.diff) > 0
sig.b <- cond1.b * cond2.b * cond3.b

strat <- tail(rs.returns, -1) * head(sig.b - sig.s, -1)
strat.perf <- cumsum(na.omit(strat))
plot(strat.perf)
tail(strat.perf)
