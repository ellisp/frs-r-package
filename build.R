library(devtools)
library(knitr)
knit("README.Rmd")

document("pkg")
build("pkg")
