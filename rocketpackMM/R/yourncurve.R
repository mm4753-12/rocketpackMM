#' YOURNCURVE
#'
#' @param A value for a variable distributed normally, a mean, and a standard deviation.
#'
#' @return A density curve, an area, and a probability.
#' @export
#'
#' @examples
#' \dontrun yourncurve(6,10,5)
yourncurve = function(Y,mu,sigma){
  curve(dnorm(x,mean=mu,sd=sigma),xlim=c(mu-3*sigma,mu+3*sigma))
  xcurve=seq(Y-10*sigma, Y, length=10000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(Y-10*sigma,xcurve,Y),c(0,ycurve,0),col="Light Green")
  list(round(pnorm(Y,mu,sigma),4))
}
