# Manual: http://rpubs.com/michelenuijten/statcheckmanual
library(statcheck)

# From text
txt <- "blablabla the effect was very significant (t(48) = 1.02, p < .05)"
statcheck(txt)

# From PDF
setwd("C:/Users/Peter/Documents/GU/code/rcode/has-demo")
file <- "ecrea-paper.pdf"

# Extract stats
stats <- checkPDF(file)

stats
plot(stats)

#checkPDF() # välj pdf manuellt
