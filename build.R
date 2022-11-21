library(devtools)
library(knitr)
library(extrafont)
knit("README.Rmd")

source("processing/prep-pac-map.R")

document("pkg")
run_examples("pkg")

sapply(c("p1.png", "p1.svg", "p2.png", "p2.svg", "p3.svg", "p3.png"), unlink)

check("pkg")
build("pkg")

unlink("test.svg")
