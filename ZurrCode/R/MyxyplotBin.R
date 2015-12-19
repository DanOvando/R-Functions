#' MyxyplotBin

MyxyplotBin <- function(Z, MyV, NameY1) {
  AllX  <- as.vector(as.matrix(Z[,MyV]))
  AllY  <- rep(Z[,NameY1] , length(MyV))
  AllID <- rep(MyV, each = nrow(Z))


  library(mgcv)
  library(lattice)

  P <- xyplot(AllY ~ AllX | factor(AllID), col = 1,
              strip = function(bg='white', ...) strip.default(bg='white', ...),
              scales = list(alternating = T,
                            x = list(relation = "free"),
                            y = list(relation = "same")),
              xlab = "Covariate",
              ylab = "Probability of presence",
              panel=function(x,y){
                panel.grid(h=-1, v= 2)
                panel.points(x,y,col=1)
                tmp<-gam(y~s(x, k = 4), family = binomial)
                MyData <- data.frame(x = seq(min(x), max(x), length = 25))
                p1 <- predict(tmp, newdata = MyData, type ="response")
                panel.lines(MyData$x,p1, col = 1, lwd = 3)
              })

  print(P)
}
