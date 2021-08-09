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

# GARCH BOOTSTRAP
library(rugarch)
uspec <- ugarch.spec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = "std"))
fit.garch <- ugarchfit(spec = uspec, data = logret[, 1])
# estimate parameters are in
fit.garch@fit$coef

# save output estimation
save1 <- cbind(logret[, 1], fit.garch@fit$sigma, fit.garch@fit$z)
names(save1) <- c("logret", "s", "z")

# simulate 1-day outcome
set.seed(123789)
boot.garch <- ugarchboot(fit.garch,
                         method = c("Partial", "Full"[1]), #ignore parameter uncertainty
                         sampling = "raw", #draw from standardised residuals
                         n.ahead = 1, #1-day ahead
                         n.bootpred = 100000, #number of simulated outcomes
                         solver = "solnp")
# save in vector rvec
rvec <- boot.garch@fseries
VaR <- quantile(rvec, 0.05)
ES<-mean(rvec[rvec<VaR])
