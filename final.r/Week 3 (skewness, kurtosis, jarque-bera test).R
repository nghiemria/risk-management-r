library(quantmod)

# Create data series called "wilsh"
wilsh <- getSymbols("GOLDPMGBD228NLBM", src = "FRED", auto.assign = FALSE)
wilsh <- na.omit(wilsh)
wilsh <- wilsh["1979-12-31/2017-12-31"]
name(wilsh) <- "TR"
# View the first observation is an "NA"
logret <- diff(log(wilsh))
head(logret, 3)

# Remove "NA" and calculate log returns
logret <- diff(log(wilsh))[-1]

# ESTIMATE PARAMETERS OF THE SCALED STUDENT T-DISTRIBUTION
# install "moments" package
library(MASS)
rvec <- as.vector(logret)

#skewness
round(skewness(rvec), 2)
#kurtosis
round(kurtosis(rvec), 2)
#jacque-bera test of normality
jarque.test(rvec)