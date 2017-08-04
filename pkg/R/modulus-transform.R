


#' Modulus transform
#'
#' John and Draper's modulus transformation
#' 
#' @export
#' @import scales
#' @import ggplot2
#' @param lambda parameter to pass to the underlying modulus transformation
#' @details Creates a function to execute a Box-Cox power transformation to the absolute value of a variable and then restores its sign.
#' @return An object of class "trans", which encapsulates the transformation and its inverse
#' @references  
#' J. A. John and N. R. Draper, "An Alternative Family of Transformation", 
#' \emph{Journal of the Royal Statistical Society. Series C (Applied Statistics)} 
#' Vol. 29, No. 2 (1980) pp. 190-197
#' 
#' \url{http://ellisp.github.io/blog/2015/09/05/creating-a-scale-transformation}
#' @author Peter Ellis
#' @examples
#' eg_data <- data.frame(x = exp(rnorm(1000)) * 
#'                sample(c(-1, 1), 1000, replace = TRUE, prob = c(0.2, 0.8)))
#' 
#' ggplot(eg_data, aes(x = x)) +
#'   geom_density() +
#'   scale_x_continuous("Transformed scale",
#'                      trans = modulus_trans(0.1),
#'                      breaks = modulus_breaks(lambda = 0.1))
modulus_trans <- function(lambda){
  scales::trans_new("modulus",
            transform = function(y){
              if(lambda != 0){
                yt <- sign(y) * (((abs(y) + 1) ^ lambda - 1) / lambda)
              } else {
                yt = sign(y) * (log(abs(y) + 1))
              }
              return(yt)
            },
            inverse = function(yt){
              if(lambda != 0){
                y <- ((abs(yt) * lambda + 1)  ^ (1 / lambda) - 1) * sign(yt)
              } else {
                y <- (exp(abs(yt)) - 1) * sign(yt)
                
              }
              return(y)
            }
  )
}

#' Modulus transform
#' 
#' Convenience version of this modulus transform function
#' @param y variable to be transformed
#' @param lambda parameter passed to the Box-Cox transformation
#' @author Peter Ellis
.mod_transform <- function(y, lambda){
  if(lambda != 0){
    yt <- sign(y) * (((abs(y) + 1) ^ lambda - 1) / lambda)
  } else {
    yt = sign(y) * (log(abs(y) + 1))
  }
  return(yt)
}

#' Modulus transform
#' 
#' Convenience version of the inverse of this modulus transform function
#' @param yt variable to be inverse-transformed
#' @param lambda parameter passed to the Box-Cox transformation
#' @author Peter Ellis
.mod_inverse <- function(yt, lambda){
  if(lambda != 0){
    y <- ((abs(yt) * lambda + 1)  ^ (1 / lambda) - 1) * sign(yt)
  } else {
    y <- (exp(abs(yt)) - 1) * sign(yt)
    
  }
  return(y)
}

#' Prettify
#' 
#' Turn a set of numbers into a rounded set
#' 
#' @export
#' @param x numbers to be rounded
#' @details Rounding is more aggressive the larger the numbers are.  Expected usage is for axis labels or similar.
#' @return Rounded numbers
#' @examples
#' prettify(c(1, 8.3, 98.78987))
prettify <- function(x){
  digits <- -floor(log10(abs(x))) + 1
  digits[x == 0] <- 0
  return(round(x, digits = digits))
}

#' Modulus transform breaks
#'
#' Create a function to calculate sensible breaks for axis labels in combination with a modulus transform
#' @export
#' @param lambda parameter for modulus transform
#' @param n number of axis labels expected
#' @param prettify whether or not to prettify the axis labels
#' @return a function to calculate breaks, suitable for use in conjunction with the scales package
#' @seealso \code{\link{prettify}}, \code{\link{modulus_trans}}
#' @references Inspired by Heather Turner's answer to a Stack Overflow question at 
#' \url{https://stackoverflow.com/questions/14255533/pretty-ticks-for-log-normal-scale-using-ggplot2-dynamic-not-manual}
#' @author Peter Ellis
#' @examples
#' eg_data <- data.frame(x = exp(rnorm(1000)) * 
#'                sample(c(-1, 1), 1000, replace = TRUE, prob = c(0.2, 0.8)))
#' 
#' ggplot(eg_data, aes(x = x)) +
#'   geom_density() +
#'   scale_x_continuous("Transformed scale",
#'                      trans = modulus_trans(0.1),
#'                      breaks = modulus_breaks(lambda = 0.1)) 
modulus_breaks <- function(lambda, n = 8, prettify = TRUE){
  function(x){
    breaks <- .mod_inverse(pretty(.mod_transform(x, lambda = lambda), n = n), lambda = lambda)
      
    if(prettify){
      breaks <- prettify(breaks)
    }
    return(breaks)
  }
}
