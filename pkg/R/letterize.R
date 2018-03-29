

#' Letterize
#' 
#' Convert levels of a factor or character to letters, for anonymisation
#' 
#' @param x
#' @param method
#' @param output
#' @details
#' @value
#' @export
#' 
#' @examples
#' x <- c("Peter", "John", "Mary", "Jane", "John")
#' letterize(x)
#' letterize(x, output = "factor")
#' letterize(x, method = "frequency")
#' letterize(x, method = "original") # alphabetical
letterize <- function(x, method = c("random", "original", "frequency"), output = c("original", "character", "factor")){
  original_class <- class(x)
  method <- match.arg(method)
  output <- match.arg(output)
  x <- as.factor(x)
  nl <-  length(levels(x))
  if (nl > 26){
    stop("Too many distinct values in x, can't replace them with letters")
  }
  if(method == "random"){
    levels(x) <- sample(LETTERS[1:nl])  
  }
  if(method == "original"){
    # note this will allocate A, B, C in the original order of factor levels, or alphabetical if x was a character vector
    levels(x) <- LETTERS[1:nl]
  }
  if(method == "frequency"){
    xt <- sort(table(x), decreasing = TRUE)
    x <- factor(x, levels = names(xt))
    levels(x) <- LETTERS[1:nl]
  }
  
  if(output == "character" || ("character" %in% original_class & output == "original")){
    x <- as.character(x)
  }
  return(x)
}

