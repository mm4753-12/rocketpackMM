#' ZSCORE
#'
#' @param A numeric vector of values.
#'
#' @return A Z-score quotient.
#' @export
#'
#' @examples
#' \dontrun zscore(c(1.1,1.2,1.3,1.4,1.5))
zscore <- function(x){
  (x-mean(x))/sd(x)
  }

