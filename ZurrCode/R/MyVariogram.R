#' MyVariogram
#' \code{MyVariogram}

MyVariogram <- function(x,y,z, MyDistance) {
  library(gstat)
  mydata      <- data.frame(z, x, y)
  coordinates(mydata)    <- c("x", "y")
  Var <- variogram(z ~ 1, mydata, cutoff = MyDistance)
  data.frame(Var$np, Var$dist, Var$gamma)
}