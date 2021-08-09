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
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate, 6)

# estimate VaR and ES at 95% confidence level
alpha <- 0.05
set.seed(123789)
library(metRology)
rvec <- rt.scaled(1000, mean = t.fit$estimate[1], sd = t.fit$estimate[2], t.fit$estimate[3])
VaR <- quantile(rvec, alpha)
ES <- mean(rvec[rvec < VaR])
round(VaR, 6)
round(ES,6)
