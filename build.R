library(devtools)
library(knitr)

knit("README.Rmd", "README.md")

document("pkg")
check("pkg")


