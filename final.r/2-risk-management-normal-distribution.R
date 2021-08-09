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

# ESTIMATING PARAMETERS OF THE NORMAL DISTRIBUTION
# assuming that daily log returns are normally distributed, we estimate mean and standard deviation
mu<-round(mean(logret), 8)
sig<-round(sd(logret), 8)

# estimating Value-At-Risk (VaR) of normal distribution
# calculating VaR at 95% confidence level
var <- qnorm(0.05, mu, sig)
# assuming the portfolio size is 1000 million
HFvar <- 1000 * (exp(var) - 1)

#estimating the Expected Shortfall (ES) of normal distribution
es <- mu - sig * dnorm(qnorm(0.05, 0, 1), 0, 1) / 0.05
HFvar<-1000*(exp(es)-1)
