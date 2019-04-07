


#' Add Google webfonts to the style definition of an SVG file
#' 
#' @param svgfile path to an SVG
#' @param googlefonts Google Web fonts to be added
#' @param new_svgfile path to a new SVG file to create. By default will overwrite \code{svgfile}.
#' @author Peter Ellis
#' @details This is a very quick hack put together for my purposes, I don't know how generally well 
#' it will work.
#' @export
#' @examples 
#' \dontrun{
#' library(svglite)
#' svglite("test.svg")
#' par(family =  "Indie Flower")
#' plot(1:10, 1:10, main = "Indie Flower", xlab = "X axis here")
#' text(10:1, 1:10, LETTERS[1:10])
#' dev.off()
#' svg_googlefonts("test.svg", "Indie Flower")
#' }
svg_googlefonts <- function(svgfile, googlefonts = c("Roboto", "Sarala"), new_svgfile = svgfile){
  
  style_string <- paste0("<style>\n",
                         paste0(
                           "  @import url('https://fonts.googleapis.com/css?family=", 
                           googlefonts, 
                           ":400,400i,700,700i');", collapse = "\n"),
                         "\n</style>")
  
  txt <- readChar(svgfile, file.info(svgfile)$size)
  
  txt <- gsub("</style>", paste0("</style>\n", style_string), txt)
  writeChar(txt, new_svgfile, eos = NULL)
}
