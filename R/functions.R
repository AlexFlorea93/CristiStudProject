

checkDependencies<-function()
{
print('Dependencies on ggplot2, gridExtra, grid')
check<-as.data.frame(installed.packages())
check<-data.frame(check$Package,1)
if(check$X1[check$check.Package=='ggplot2']==1)
{
  print('calling ggplot2')
  library(ggplot2)
}else
{
  print('installing ggplot2')
  install.packages('ggplot2')
  print('calling ggplot2')
  library(ggplot2)
}

if(check$X1[check$check.Package=='gridExtra']==1)
{
  print('calling gridExtra')
  library(gridExtra)
}else
{
  print('installing gridExtra')
  install.packages('gridExtra')
  print('calling gridExtra')
  library(gridExtra)
}



if(check$X1[check$check.Package=='grid']==1)
{
  print('calling grid')
  library(grid)
}else
{
  print('installing grid')
  install.packages('grid')
  print('calling grid')
  library(grid)
}

}

#library(ggplot2)
#library(gridExtra)
#library(grid)

CLT.generator <- function(n, K) {
  U <- runif(n * K, min = 0, max = 1)
  Y <- rowMeans(matrix(U, nrow = n, ncol = K))
  return(Y)
}

BoxMuller.generator <- function(n) {
  U1 <- runif(n, min = 0, max = 1)
  U2 <- runif(n, min = 0, max = 1)
  a <- sqrt (-2 * log(U1))
  b <- 2 * pi * U2
  Y <- a * sin(b)
  Y2 <- a * cos(b)
  return(Y)
}

Leva.generator <- function(n) {
  s <- 0.449871
  t <- -0.386595
  r1 <- 0.27597
  r2 <- 0.27846
  a <- 0.196
  b <- 0.25472
  U1 <- runif(n, min = 0, max = 1)
  V1 <- runif(n, min = -1, max = 1)
  u <- U1
  v <- sqrt(2 / exp(1)) * V1
  x <- u - s
  y <- abs(v) - t
  Q <- x ^ 2 + y * (a * y - b * x)
  Y <- 0
  for (i in 1:n) {
    u <- U1[i]
    v <- sqrt(2 / exp(1)) * V1[i]
    x <- u - s
    y <- abs(v) - t
    Q <- x ^ 2 + y * (a * y - b * x)
    if (Q < r1) {
      Y[i] <- v / u
    } else if (Q < r2) {
      if (v ^ 2 < 4 * u ^ 2 * log(u)) {
        Y[i] <- v / u
      }
   }
  }
  Y <- Y[!is.na(Y)]
  return(Y)
}

create.plots <- function(Y, title, n, time) {
  p1 <- qplot(Y, geom = "histogram", bins = 50, main = "Histogram")
  p2 <- qqplot.data(Y)
  grid.arrange(p1, p2, ncol=2, top = textGrob(paste(title,", n =",toString(n),", runtime =",toString(time),"s"), gp=gpar(fontsize=18,font=8)))
}

qqplot.data <- function(vec) {
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  d <- data.frame(resids = vec)
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int, colour="red") + ggtitle("Q-Q plot")
}
