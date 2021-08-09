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

# SIMULATION METHOD 1
mu <- mean(logret)
sig <- sd(logret)
# 100,000 outcomes
set.seed(123789)
rvec <- rnorm(100000, mu, sig)
# VaR at 95% confidence level is 5% quantile of 100,000 outcomes
VaR <- quantile(rvec, 0.05)
# ES at 95% confidence level is average of 100,000 outcomes worse than VaR
ES <- mean(rvec[rvec < VaR])

# SIMILATION METHOD 2
set.seed(123789)
rvec <- sample(as.vector(logret), 100000, replace = TRUE)
VaR <- quantile(rvec, 0.05)
ES<-mean(rvec[rvec<VaR])
