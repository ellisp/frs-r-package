

x <- c(4,6,8,9,7,5)
cvv <- c(0, 0.3, 0.5, 0.2)

convolution <- function(x, cvv){
  
  cvd <- data.frame(cvv = cvv, link = 1, lag = 0:(length(cvv) - 1))
  orig <- data.frame(x = c(x, rep(0, length(cvv) - 1)), link = 1)
  orig$location <- 1:nrow(orig)
  combined <- dplyr::left_join(orig, cvd, by = "link")
  combined$z <- with(combined, x * cvv)
  combined$new_location <- with(combined, location + lag)
  y <- aggregate(combined$z, list(combined$new_location), sum)$x
  
  # The above procedure will always finish with a tail of zeroes
  y <- head(y, length(y) - length(cvv) + 1)
  return(y)
}

convolution(x, cvv)
