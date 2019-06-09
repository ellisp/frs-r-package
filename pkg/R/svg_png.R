#' Draw a graphic as both an SVG and a PNG
#' 
#' 
#' @param file stub of filename under which to save the graphic. .svg and .png will be added automatically.
#' @param res resolution in dots per inch, to be passed to \code{Cairo::CairoPNG}
#' @param googlefonts vector of names of Google Fonts to be linked to from the SVG, to be passed
#' to \code{frs::svg_googlefonts}
#' @param p ggplot2 object to draw, or base plot as a function
#' @param w width in inches
#' @param h height in inchest
#' @details Under the hood, this uses CairoPNG, svglite, and svg_googlefonts to produce a high
#' quality SVG and an identical (hopefully) high quality PNG
#' @importFrom svglite svglite
#' @importFrom Cairo CairoPNG
#' @importFrom grDevices dev.off
#' @export
#' @examples 
#' if(require(ggplot2)){
#'   p1 <- ggplot(mtcars, aes(x = disp, y = mpg)) + geom_point()
#'   svg_png("p1", p1)
#'   }
#' 
#' p2 <- function(){plot(1:10, 1:10)}
#' svg_png("p2", p2)
#' 
#' if(require(lattice)){
#'   Depth <- equal.count(quakes$depth, number=8, overlap=.1)
#'   p3 <- xyplot(lat ~ long | Depth, data = quakes)
#'   svg_png("p3", p3)
#'   }
#' 
svg_png <- function(file, p, w = 8, h = 5, res = 600, googlefonts = c("Roboto", "Sarala")){
  
  plot_drawer <- function(){
    if("ggplot" %in% class(p) |
       "trellis" %in% class(p)){
      print(p)
    } else if("function" %in% class(p)){
      p()
    } else {
      stop("p must be a function,a ggplot2 object or a lattice object")
    }
  }
  
  svgfile <- paste0(file, ".svg")
  
  svglite::svglite(svgfile,
                   width = w, 
                   height = h)
    plot_drawer()
    grDevices::dev.off()
  
  svg_googlefonts(svgfile)
  
  Cairo::CairoPNG(paste0(file, ".png"),
           width = w * res,
           height = h * res,
           res = res)
    
    plot_drawer()
  
    grDevices::dev.off()
  
}


