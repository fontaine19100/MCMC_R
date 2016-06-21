library(mcmc)
library(mvtnorm)
my.seed <- 123

logprior<-function(X,...)
{
  ifelse( (-50.0 <= X[,1] & X[,1]<=50.0) & (-50.0 <= X[,2] & X[,2]<=50.0), return(0), return(-Inf))
}

logpost<-function(X,...){
  log.like <- log( exp(-((X[,1]^2 + X[,2]^2 - 4)/10 )^2) * sin(4*atan(X[,2]/X[,1])) )
  log.prior<-logprior(X)
  log.post<-log.like + log.prior # if flat prior, the posterior distribution is the likelihood one
  return (log.post)
}
# 
# logprior<-function(x,y,...)
# {
#   ifelse( (-50.0 <= x & x<=50.0) & (-50.0 <= y & y<=50.0), return(0), return(-Inf))
# }
# 
# logpost<-function(x,y,...){
#   log.like <- log( exp(-((x^2 + y^2 - 4)/10 )^2) * sin(4*atan(y/x)) )
#   log.prior<-logprior(x,y)
#   log.post<-log.like + log.prior # if flat prior, the posterior distribution is the likelihood one
#   return (log.post)
# }

x <- seq(-5,5,0.15)
y <- seq(-5,5,0.15)
X<-cbind(x,y)

#dmvt(X, df=3, log=TRUE)
#out <- metrop(function(X) dmvt(X, df=3, log=TRUE), 0, blen=100, nbatch=100)
out <- metrop(function(X) logpost(X), c(0,0), blen=100, nbatch=100)
#out <- metrop(function(x) dt(x, df=3, log=TRUE), 0, blen=100, nbatch=100)
out <- metrop(out)
out$accept