# Moving averages to measure the rate of a Poisson event. 

SMA.run <- function (width) {
  
  force(width)
  data <- NULL
  
  clipped <- function (time) {
    data[data > time - width]
  }
  
  list(
    update = function (time) {
      data <<- c(clipped(time), time)},
    read = function (time) {
      data <<- clipped(time)
      length(data) / min(width, time)}
  )
}


EMA.run <- function (width) {
  
  force(width)
  count <- 0
  last <- 0
  
  advance <- function (time) {
    count <<- count * exp((last - time) / width)
    last <<- time
  }
  
  list(
    update = function (time) {
      advance(time)
      count <<- count + 1},
    read = function (time) {
      advance(time)
      count / width / (1 - exp(-time / width))}
  )
}