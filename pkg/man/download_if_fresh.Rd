% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_if_fresh.R
\name{download_if_fresh}
\alias{download_if_fresh}
\title{Download a file if it doesn't already exist locally}
\usage{
download_if_fresh(fn, destfile, mode = "wb", ...)
}
\arguments{
\item{fn}{URL of a file to download}

\item{destfile}{path and name of file to save the file as, if it isn't already there}

\item{mode}{mode of the file to be downloaded. See \code{\link[utils]{download.file}} for details.}

\item{...}{other arguments to be passed to \code{download.file()}}
}
\value{
An (invisible) integer code, 987 if no attempt was made to download the file (because \code{destfile}
already exists), 0 for success and non-zero for failure. Inherited from \code{download.file} if the file 
is downloaded.
}
\description{
Download a file if it doesn't already exist locally
}
\details{
This is a simple convenience function that only downloads files if the \code{destfile} doesn;t already exist.
It should be used in situations where you want, for reproducibility, to ensure a file is downloaded but you 
don't want it freshly downloaded every time a project is run (typically, because it is large, and not 
updated frequently)
}
\examples{
file <- "_dif_eg.html"
x <- download_if_fresh("http://freerangestats.info", destfile = file)
y <- download_if_fresh("http://freerangestats.info", destfile = file)
print(c(x, y))
unlink(file)
}
\seealso{
\code{\link[utils]{download.file}}
}
\author{
Peter Ellis
}
