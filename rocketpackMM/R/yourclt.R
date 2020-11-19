#' YOURCLT
#'
#' @param n (a size)
#' @param iter (iteration count)
#' @param a (a lower limit)
#' @param b (an upper limit)
#'
#' @return a histogram and a summation of "iter" columns
#' @export
#'
#' @examples
#' \dontrun yourclt(n=10,iter=10000)
yourclt=function(n=10,iter=100,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
