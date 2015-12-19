#' MyxyplotPolygon
#'

MyxyplotPolygon <- function(Z, MyV, NameY1) {
  AllX  <- as.vector(as.matrix(Z[,MyV]))
  AllY  <- rep(Z[,NameY1] , length(MyV))
  AllID <- rep(MyV, each = nrow(Z))


  library(mgcv)
  library(lattice)
  Z <- xyplot(AllY ~ AllX|factor(AllID), col = 1,
              xlab = list(label = "Explanatory variables", cex = 1.5),
              ylab = "",
              strip = function(bg='white',cex.lab = 1.5,...)
                strip.default(bg='white', ...),
              scales = list(alternating = T,
                            x = list(relation = "free"),
                            y = list(relation = "same")),
              panel=function(x, y){
                t1 <- gam(y~s(x))
                MD1 <- data.frame(x=seq(from = min(x, na.rm = TRUE),
                                        to = max(x, na.rm = TRUE),
                                        length = 100))
                P1 <- predict(t1,   se.fit = TRUE)
                I1 <- order(x)
                xs <- sort(x)
                panel.lines(xs, P1$fit[I1], col = 1)
                panel.polygon(c(xs, rev(xs)),
                              c(P1$fit[I1]-2*P1$se.fit[I1],
                                rev(P1$fit[I1]+2*P1$se.fit[I1])),
                              col = gray(0.7),
                              density = 10 )
                panel.grid(h=-1, v= 2)
                panel.abline(0,0)
                panel.points(x, y, col = 1)

              })
  #Because the xyplot is inside a function you need to print
  #construction below
  print(Z)
}
