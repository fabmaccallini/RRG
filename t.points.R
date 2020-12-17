t.points <- function (x, radius = 5, sd = 1, symmetry = TRUE)
{
  # The function returns a matrix with the turning points.
  # The data are processed by column and by row in ascending order, returning -1 for local minima and 1 for local maxima.
  #
  # Inputs:
  # x         vector or matrix of data points
  # radius    number of data points to look forward and backward to elect a turning point
  # sd        number of standard deviations away within the given radius to elect a turning point
  # symmetry  if TRUE, any local minimum/maximum strictly follows a local maximum/minimum
  
  if (!is.matrix(x)) x <- data.matrix(x)
  dim1 <- dim(x)[1]
  dim2 <- dim(x)[2]
  minmax <- matrix(0, dim1, dim2)
  require (TTR)
  ifelse(sd = 0, 0, dev <- apply(x, 2, runSD, n = radius * 2) * sd)
  for (j in 1: dim2) {
    last <- 1
    if (symmetry == TRUE) {
      for (i in (radius + 1): (dim1 - radius)) {
        # local maximum
        if ((x[i, j] == max(x[(i - radius): (i + radius), j])) # equal to maximum
            & any(x[(i - radius): (i - 1), j] < (x[i, j] - dev[i, j]))    # deviation higher right
            & any(x[(i + 1): (i + radius), j] < (x[i, j] - dev[i, j]))) { # deviation higher left
          # check with previous turning point
          if (x[i, j] > x[last, j]) {
            minmax[i, j] <- 1
            if (minmax[last, j] == 1) minmax[last, j] <- 0
            last <- i
          # local maximum lower than previous local minimum not counted
          } else minmax[i, j] <- 0
        # local minimum
        } else if ((x[i, j] == min(x[(i - radius): (i + radius), j])) # equal to minimum
                   & any(x[(i - radius): (i - 1), j] >= (x[i, j] + dev[i, j]))    # deviation lower right
                   & any(x[(i + 1): (i + radius), j] >= (x[i, j] + dev[i, j]))) { # deviation lower left
          # check with previous turning point
          if (x[i, j] < x[last, j]) {
            minmax[i, j] <- -1
            if (minmax[last, j] == -1) minmax[last, j] <- 0
            last <- i
            # local minimum higher than previous local maximum not counted
          } else minmax[i, j] <- 0
        }
      }
    } else {
      for (i in (radius + 1): (dim1 - radius)) {
        if ((x[i, j] == max(x[(i - radius): (i + radius), j])) # equal to maximum
            & any(x[(i - radius): (i - 1), j] < (x[i, j] - dev[i, j]))  # deviation higher right
            & any(x[(i + 1): (i + radius), j] < (x[i, j] - dev[i, j]))) # deviation higher left
           minmax[i, j] <- 1
        else if ((x[i, j] == min(x[(i - radius): (i + radius), j])) # equal to minimum
                 & any(x[(i - radius): (i - 1), j] > (x[i, j] + dev[i, j]))  # deviation lower right
                 & any(x[(i + 1): (i + radius), j] > (x[i, j] + dev[i, j]))) # deviation lower left
                minmax[i, j] <- -1
      }
    }
  }

  return (minmax)
}