library(devtools)
library(knitr)
library(extrafont)
knit("README.Rmd")

document("pkg")
run_examples("pkg")
check("pkg")
build("pkg")

unlink("test.svg")
