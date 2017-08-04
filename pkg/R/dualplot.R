#' Dual scale time series plot
#' 
#' Time series line chart with two different vertical scales
#' 
#' @export dualplot
#' @import stats
#' @import graphics
#' @author Peter Ellis, \email{peter.ellis2013nz@gmail.com}
#' @references \url{http://ellisp.github.io/blog/2016/08/18/dualaxes}, \url{http://ellisp.github.io/blog/2016/08/28/dualaxes2}
#' @param x1 x coordinates for first series
#' @param y1 y coordinates for first series
#' @param y2 y coordinates for second series
#' @param x2 x coordinates for second series.  Defaults to be the same as x1 (ie both series covering the same time periods)
#' @param col vector of two colours for the two series
#' @param lwd vector of two line widths (as in \code{par})
#' @param colgrid colour of gridlines to be drawn (if NULL, none are drawn)
#' @param mar argument passed through to \code{par}
#' @param ylab1 label for left y axis, for first series
#' @param ylab2 label for right y axis, for second series
#' @param nxbreaks number of breaks to split the x axis into
#' @param yleg1 label for first series in the legend
#' @param yleg2 label for second series in the legend
#' @param ylim1 Override \code{ylim} passed to \code{plot} for the first series.  Not recommended.
#' @param ylim2 Override \code{ylim} passed to \code{plot} for the second series.  Not recommended.
#' @param ylim.ref vector of two, indicating which element in the first series and the second series to use as the reference for converting to an index for purpose of determining ylim.  Only used if all y1 and y2 are positive.
#' @param xlab label for x (horizontal) axis 
#' @param main main title for plot
#' @param legx passed through to \code{legend}, controls position of legend
#' @param legy passed through to \code{legend}
#' @param bty passed through to \code{legend} 'o' or 'n' indicates whether or not (respectively) to draw a box around the legend
#' @param silent suppress warnings and messages?
#' @param ... other arguments passed through to \code{legend}
#' @return NULL
#' @details Draws a time series line chart with two different vertical scales.  If defaults are used, \code{dualplot()} 
#' attempts to derive sensible values of ylim1 and ylim2 that minimise the misinterpretation common to dual axis time
#' series charts.  In particular:
#' \enumerate{
#'  \item if all values of y1 and y2 are > 0, the y limits are chosen so the two series are drawn as though they had been converted to indexes, using the \code{ylim.ref[1]}th element of y1  and \code{ylim.ref[2]}th element of y2 as their reference points.
#'    \itemize{
#'      \item If y1 and y2 are equal lengths, ylim.ref defaults to 1, 1
#'      \item If y1 and y2 are unqual lengths, ylim.ref takes the values that make the two series cross over at the point in time where the later starting series begins.
#'  }
#'  \item if either y1 or y2 goes to zero or below, the y limits are chosen to be the mean of each series +/- 3.5 standard deviations
#' }
#' 
#' The data should be sorted (earliest to most recent) before use or odd choices might be made.
#' 
#' @seealso \code{\link{par}}
#' @examples
#' 
#' data(dairy)
#' data(fonterra)
#' # with defaults for positioning scales, but exemplifying custom colours, etc:
#' dualplot(x1 = dairy$Date, y1 = dairy$WMP_Total, 
#'          x2 = fonterra$Date, y2 = fonterra$FCGClose,
#'          ylab1 = "Whole milk powder price index\n",
#'          ylab2 = "Fonterra Cooperative Group\nshare price ($)",
#'          col = c("#AA6800", "#7869CD"),
#'          colgrid = "grey90",
#'          main = "Fonterra share prices are less volatile than global milk prices")
#' 
#' # draw as though each series indexed, starting from its first point:
#' dualplot(x1 = dairy$Date, y1 = dairy$WMP_Total, 
#'          x2 = fonterra$Date, y2 = fonterra$FCGClose,
#'          ylim.ref = c(1, 1))
#' 
#' # draw as though each series indexed, ending at the same point:
#' dualplot(x1 = dairy$Date, y1 = dairy$WMP_Total, 
#'          x2 = fonterra$Date, y2 = fonterra$FCGClose,
#'          ylim.ref = c(nrow(dairy), nrow(fonterra)))
#' 
#' # pretend one of the series is negative and see how the axes are chosen differently:
#' dualplot(x1 = dairy$Date, y1 = -dairy$WMP_Total, 
#'          x2 = fonterra$Date, y2 = fonterra$FCGClose)
#' # note the plot above is *not* meaningful - just for illustrating negatives.
dualplot <- function(x1, y1, y2, x2 = x1, 
                     col = c("#C54E6D", "#009380"),
                     lwd = c(1, 1), colgrid = NULL,
                     mar = c(3, 6, 3, 6) + 0.1, 
                     ylab1 = paste(substitute(y1), collapse = ""), 
                     ylab2 = paste(substitute(y2), collapse = ""),
                     nxbreaks = 5, 
                     yleg1 = paste(gsub("\n$", "", ylab1), "(left axis)"), 
                     yleg2 = paste(ylab2, "(right axis)"),
                     ylim1 = NULL, ylim2 = NULL, ylim.ref = NULL,
                     xlab = "", main = NULL, legx = "topleft", legy = NULL, 
                     bty = "n", silent = FALSE, ...){
  # Base graphics function for drawing dual axis line plot.
  # Assumed to be two time series on a conceptually similar, non-identical scale 
  #
  # Assumes data is in sequence of x1 and of x2 ie ordered by time
  #
  # Use with caution! 
  # Please don't use to show growth rates and the original
  # series at the same time!
  #
  # Peter Ellis, 16-27 August 2016, GNU GPL-3
  # Note that default colours were chosen by colorspace::rainbow_hcl(2, c = 80, l = 50)
  
  # strip excess attributes (eg xts etc) from the two vertical axis variables
  ylab1 <- as.character(ylab1)
  ylab2 <- as.character(ylab2)
  y1 <- as.numeric(y1)
  y2 <- as.numeric(y2)
  
  # is ylim.ref is NULL, calculate a good default
  if(is.null(ylim.ref)){
    if (length(y1) == length(y2)){
      ylim.ref <- c(1, 1)
    } else {
      if (min(x1) >  min(x2)){
        ylim.ref <- c(1, which(abs(x2 - min(x1)) == min(abs(x2 - min(x1)))))
      } else {
        ylim.ref <- c(which(abs(x1 - min(x2)) == min(abs(x1 - min(x2)))), 1)
      }
    }
    
    
  }
  
  
  oldpar <- par(mar = mar)
  xbreaks <- round(seq(from = min(c(x1, x2)), to = max(c(x1, x2)), length.out = nxbreaks))
  
  # unless ylim1 or ylim2 were set, we set them to levels that make it equivalent
  # to a graphic drawn of indexed series (if all data positive), or to the mean
  # of each series +/- three standard deviations if some data are negative
  if(is.null(ylim1) & is.null(ylim2)){
    if(min(c(y1, y2), na.rm = TRUE) <= 0){
      message("With negative values ylim1 or ylim2 need to be chosen by a method other than treating both series visually as though they are indexed. Defaulting to mean value +/- 3.5 times the standard deviations.")
      ylim1 <- c(-3.5, 3.5) * sd(y1, na.rm = TRUE) + mean(y1, na.rm = TRUE)
      ylim2 <- c(-3.5, 3.5) * sd(y2, na.rm = TRUE) + mean(y2, na.rm = TRUE)
    } else {
      
      
      if(ylim.ref[1] > length(y1)){
        stop("ylim.ref[1] must be a number shorter than the length of the first series.")
      }
      if(ylim.ref[2] > length(y2)){
        stop("ylim.ref[2] must be a number shorter than the length of the second series.")
      }
      
      if(!silent) message("The two series will be presented visually as though they had been converted to indexes.")
      
      # convert the variables to indexes (base value of 1 at the time specified by ylim.ref)
      ind1 <- as.numeric(y1) / y1[ylim.ref[1]]
      ind2 <- as.numeric(y2) / y2[ylim.ref[2]]
      
      # calculate y axis limits on the "index to 1" scale
      indlimits <- range(c(ind1, ind2), na.rm = TRUE)
      
      # convert that back to the original y axis scales
      ylim1 = indlimits * y1[ylim.ref[1]]
      ylim2 = indlimits * y2[ylim.ref[2]]
    } 
  }else {
    if(!silent) warning("You've chosen to set at least one of the vertical axes limits manually.  Up to you, but it is often better to leave it to the defaults.")
  }
  
  # draw first series - with no axes.
  plot(x1, y1, type = "l", axes = FALSE, lwd = lwd[1],
       xlab = xlab, ylab = "", col = col[1], main = main, 
       xlim = range(xbreaks), ylim = ylim1)
  
  # add in the gridlines if wanted:
  if(!is.null(colgrid)){
    grid(lty = 1, nx = NA, ny = NULL, col = colgrid)   
    abline(v = xbreaks, col = colgrid)
  }
  
  # add in the left hand vertical axis and its label
  axis(2, col = col[1], col.axis= col[1], las=1 )  ## las=1 makes horizontal labels
  mtext(paste0("\n", ylab1, "\n"), side = 2, col = col[1], line = 1.5) 
  
  # Allow a second plot on the same graph
  par(new=TRUE)
  
  # Plot the second series:
  plot(x2, y2,   xlab="", ylab="", axes = FALSE, type = "l", lwd = lwd[2],
       col = col[2], xlim = range(xbreaks), ylim = ylim2)
  
  ## add second vertical axis (on right) and its label
  mtext(paste0("\n", ylab2, "\n"), side = 4, col = col[2], line = 4.5) 
  axis(4,  col = col[2], col.axis = col[2], las=1)
  
  # Draw the horizontal time axis
  axis(1, at = xbreaks, labels = xbreaks)
  
  # Add Legend
  legend(x = legx, y = legy, legend=c(yleg1, yleg2),
         text.col = col, lty = c(1, 1), lwd = lwd, col = col,
         bty = bty, ...)
  
  par(oldpar)
}