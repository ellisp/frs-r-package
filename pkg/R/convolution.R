


#' One-dimensional convolution
#'
#' @param x a vector of counts or other numbers
#' @param cvv a vector of probabilities to convolve x at various lags. First value should be for lag 0, 
#' second for lag 1, etc. (stands for "convolution vector")
#' @param warnings whether to show warnings when the resulting vector does not add up to the
#' same sum as the original
#' @details ...
#' @return A vector of length equal to the length of x plus length of cvv minus 1
#' @export
#' @importFrom dplyr left_join
#' @examples 
#' x <- c(4,6,8,9,7,5)
#' cvv <- c(0, 0.3, 0.5, 0.2)
#' blur(x, cvv)
blur <- function(x, cvv, warnings = TRUE){
  
  if(!class(x) %in% c("integer", "numeric") || !is.null(dim(x))){
    stop("x should be a numeric vector of numbers")
  }
  
  if(!"numeric" %in% class(cvv) || !is.null(dim(cvv))){
    stop("cvv should be a numeric vector of probabilities")
  }
  
  cvd <- data.frame(cvv = cvv, link = 1, lag = 0:(length(cvv) - 1))
  orig <- data.frame(x = x, link = 1)
  orig$position <- 1:nrow(orig)
  combined <- dplyr::left_join(orig, cvd, by = "link")
  combined$z <- with(combined, x * cvv)
  combined$new_position <- with(combined, position + lag)
  y <- aggregate(combined$z, list(combined$new_position), sum)$x
  
  if(sum(x)!= sum(y) & warnings){
    warning("Something went wrong with blur; result did not sum up to original")
  }
  
  return(y)
}



#' Single dimensional deconvolution
#' 
#' @param y a vector of values to be deconvolved
#' @param cvv a vector of probabilities for the original convolution that created y
#' @param warnings passed through to blur
#' @param digits how many digits to round the deconvolved values to. If NULL no rounding occurs.
#' @details This is simply the inverse of blur. However there is no closed solution so the
#' result is estimated numerically. The column x from the data frame returned by this function is
#' a set of values that, if blur is applied to it with the cvv vector of probabilities, will
#' return y.
#' @return a data frame with columns for x (the inferred original values what were convolved to y),
#' y (which will be padded out with some extra zeroes), and position (which is the numbering of the
#' row relative to the original ordering of y; so position = 1 refers to the first value of y)
#' @export
#' @examples 
#' x <- c(4,6,8,9,7,5)
#' cvv <- c(0, 0.3, 0.5, 0.2)
#' # create a convolved version of x:
#' y <- blur(x, cvv)
#' # recover the original version of x, given its convolved version and the original convolution probabilities:
#' sharpen(y, cvv)
sharpen <- function(y, cvv, warnings = FALSE, digits = NULL){
  y2 <- c(rep(0, length(cvv)), y)
  starter_x <- c(y, rep(0, length(cvv)))
  
  fn <- function(x){
    x <- x / sum(x) * sum(y2)
    d <- sqrt(sum((blur(x, cvv, warnings = warnings)[1:length(x)] - y2) ^ 2))
    return(d)
  }
  
  op_res <- optim(starter_x, fn, lower = 0, method = "L-BFGS-B")
  
  x <- op_res$par
  x <- x / sum(x) * sum(y2)
  if(!is.null(digits)){
    x <- round(x, digits = digits)  
  }
  
  output <- data.frame(x, y = y2)
  output$position <- seq(from = length(y) - nrow(output) + 1, to = length(y), by = 1)
  return(output)
  
}


