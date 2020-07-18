


#' One-dimensional convolution
#'
#' @param x a vector of counts or other numbers
#' @param pmf a vector of probabilities to delay x at various lags. First value should be for lag 0, 
#' second for lag 1, etc. 
#' @param warnings whether to show warnings when the resulting vector does not add up to the
#' same sum as the original
#' @param scale_pmf whether or not to scale pmf so it adds exactly to one
#' @details \code{blur} and \code{sharpen} are deterministic single dimensional convolution functions
#' for simple convolution by a lagged probability mass function representing the proportion of original
#' cases that are delayed at various lags. They are for illustrative / toy purposes and probably should
#' not be used for actual analysis.
#' @return A vector of length equal to the length of x plus length of pmf minus 1
#' @export
#' @importFrom dplyr left_join
#' @examples 
#' x <- c(4,6,8,9,7,5)
#' pmf <- c(0, 0.3, 0.5, 0.2)
#' blur(x, pmf)
blur <- function(x, pmf, warnings = TRUE, scale_pmf = FALSE){
  
  if(!class(x) %in% c("integer", "numeric") || !is.null(dim(x))){
    stop("x should be a numeric vector of numbers")
  }
  
  if(!"numeric" %in% class(pmf) || !is.null(dim(pmf))){
    stop("pmf should be a numeric vector of probabilities")
  }
  
  if(scale_pmf){
    pmf <- pmf / sum(pmf)
  }
  
  cvd <- data.frame(pmf = pmf, link = 1, lag = 0:(length(pmf) - 1))
  orig <- data.frame(x = x, link = 1)
  orig$position <- 1:nrow(orig)
  combined <- dplyr::left_join(orig, cvd, by = "link")
  combined$z <- with(combined, x * pmf)
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
#' @param pmf a vector of probabilities for the original convolution that created y
#' @param warnings passed through to blur
#' @param digits how many digits to round the deconvolved values to. If NULL no rounding occurs.
#' @details \code{blur} and \code{sharpen} are deterministic single dimensional convolution functions
#' for simple convolution by a lagged probability mass function representing the proportion of original
#' cases that are delayed at various lags. They are for illustrative / toy purposes and probably should
#' not be used for actual analysis.
#' 
#' Use \code{\link[surveillance]{backprojNP}} for a better, maximum likelihood approach to recovering
#' an unseen set of original cases that result in the observations.
#' 
#' \code{sharpen} is the inverse of \code{blur}; it seeks to recover an original vector that, when blurred
#' via \code{pmf}, would produce the actual observations.
#' @return a data frame with columns for x (the inferred original values what were convolved to y),
#' y (which will be padded out with some extra zeroes), and position (which is the numbering of the
#' row relative to the original ordering of y; so position = 1 refers to the first value of y)
#' @seealso \code{\link[surveillance]{backprojNP}}.
#' @export
#' @examples 
#' x <- c(4,6,8,9,7,5)
#' pmf <- c(0, 0.3, 0.5, 0.2)
#' # create a convolved version of x:
#' y <- blur(x, pmf)
#' # recover the original version of x, given its blurred version 
#' # and the original convolution probabilities:
#' sharpen(y, pmf)
sharpen <- function(y, pmf, warnings = FALSE, digits = NULL){
  y2 <- c(rep(0, length(pmf)), y)
  starter_x <- c(y, rep(0, length(pmf)))
  
  fn <- function(x){
    x <- x / sum(x) * sum(y2)
    d <- sqrt(sum((blur(x, pmf, warnings = warnings, scale_pmf = TRUE)[1:length(x)] - y2) ^ 2))
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


