#' MultiVariogram
#'

MultiVariogram <- function(Z, MyVar, x, y, MyDistance) {
  #Z is the data frame with data
  #MyVar is a list of variables for for which variograms are calculated
  #x, y: spatial coordinates
  #MyDistance: limit for distances in the variogram

  library(lattice)
  VarAll<- c(NA,NA,NA,NA)
  for (i in MyVar){
    vi <- MyVariogram(x,y,Z[,i], MyDistance)
    vii <- cbind(vi, i)
    VarAll <- rbind(VarAll,vii)
  }
  VarAll <- VarAll[-1,]

  P <- xyplot(Var.gamma ~ Var.dist | factor(i), col = 1, type = "p", pch = 16,
              data = VarAll,
              xlab = "Distance",
              ylab = "Semi-variogram",
              strip = function(bg='white', ...)
                strip.default(bg='white', ...),
              scales = list(alternating = T,
                            x = list(relation = "same"),
                            y = list(relation = "same"))
  )

  print(P)
}
