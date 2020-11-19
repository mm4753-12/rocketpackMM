#' MYIC
#'
#' @param x (a sample vector)
#'
#' @return a 95% confidence interval
#' @export
#'
#' @examples
#' \dontrun myic(c(5.0581, 4.9707, 5.0893, 4.9334, 4.9777, 5.0285, 4.8555, 4.9565, 4.9769, 4.9722, 4.999, 4.9925, 4.9686, 5.0662, 4.9239, 4.9781, 5.0485, 5.0014, 4.9957, 5.0195, 5.0118, 4.9928, 5.0361, 5.0185, 4.9879))
myic=function(x){
  n=length(x)
  t1=qt(1-0.05/2,n-1)
  ci1=c()
  ci1[1]=mean(x)-t1*sd(x)/sqrt(n)
  ci1[2]=mean(x)+t1*sd(x)/sqrt(n)
  ci1
}
