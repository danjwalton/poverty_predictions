required.packages <- c("data.table", "survey")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/poverty_predictions")

###FUNCTIONS

#Remap survey types
remap_cov <- function(x){
  cov_dict <- c(
    "R"=1,
    "U"=2,
    "N"=3,
    "A"=NA
  )
  return(cov_dict[as.character(x)])
}
remap_cov <- Vectorize(remap_cov)

#Solve distributions for headcounts
GQsolve <- function(a,b,c,PL,mu){
  e <- -(a+b+c+1)
  m <- b^2-4*a
  n <- 2*b*e-4*c
  z <- PL/mu+b/2
  y <- 16*z^2*m-4*m^2
  x <- 16*z^2*n-4*n*m
  w <- 16*z^2*e^2-n^2
  p1 <- (-x-(sqrt(x^2-4*y*w)))/(2*y)
  p2 <- (-x+(sqrt(x^2-4*y*w)))/(2*y)
  if(p1>1 | p1<0){
    p1 <- NaN
  }
  if(p2>1 | p2<0){
    p2 <- NaN
  }
  return(data.table(GQP1=p1,GQP2=p2))
}

Betasolve <- function(a,b,c,PL,mu,init){
  p0 <- init
  N <- 10000
  tol <- 1E-15
  i <- 1
  p1 <- p0
  s <- numeric(N)
  while (i<=N) {
    f <- a*(p0^b)*((1-p0)^c)*((b/p0)-c/(1-p0))-(1-PL/mu)
    df.dx <- a*(p0^(b-2))*((1-p0)^(c-2))*((b^2)*((p0-1)^2)+b*(p0-1)*((2*c-1)*p0+1)+(c-1)*c*(p0^2))
    p1 <- (p0 - 0.01*(f/df.dx))
    s[i] <- p1
    i <- i + 1
    if (abs(p1-p0) < tol) {break}
    p0 <- p1
  }
  return(data.table(BetaP1=s[i-1]))
}

som <- fread("project_data/SOM_hh.csv")

pl <- unique(som$plinePPP)
growth.2017.2018 <- 1.02436

p20line <- 2.877820132*(pl/1.9)/growth.2017.2018
pl <- pl/growth.2017.2018

som[, p20.poor := ifelse(tc_imp <= p20line, "P20", "Not P20")]

p20 <- dcast(som, ind_profile ~ p20.poor, value.var = "weight", fun.aggregate = sum)
p20[, total_weight := P20 + `Not P20`][, `:=` (P20 = P20/total_weight, `Not P20` = NULL)]

fwrite(p20, "output/Somalia subnational P20 2018.csv")
