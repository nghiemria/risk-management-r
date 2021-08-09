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
library(MASS)
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate, 6)
#simulated ten 1-day outcomes
alpha <- 0.05
set.seed(123789)
rvec <- rep(0, 100000)
for (i in 1:10) {
    rvec<-rvec+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
VaR <- quantile(rvec, alpha)
ES <- mean(rvec[rvec < VaR])

# SIMULATION METHOD 2
alpha <- 0.05
set.seed(123789)
rvec <- rep(0, 100000)
for (i in 1:10) {
    rvec<-rvec+sample(as.vector(logret),100000,replace = TRUE)
}
VaR <- quantile(rvec, alpha)
ES <- mean(rvec[rvec < VaR])

# SIMULATION METHOD 3
alpha <- 0.05
set.seed(123789)
rdat <- as.vector(logret)
rvec <- rep(0, 100000)
posn <- seq(from = 1, to = length(rdat) - 9, by = 1)
rpos <- sample(posn, 100000, replace = TRUE)
for (i in 1:10) {
    rvec <- rvec + rdat[rpos]
    rpos<-rpos+1
}
VaR <- quantile(rvec, alpha)
ES <- mean(rvec[rvec < VaR])
